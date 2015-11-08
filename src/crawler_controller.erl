-module(crawler_controller).

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).

-export([process_response/2]).


start_link(Url, WorkersCount) ->
    gen_server:start_link(?MODULE, [Url, WorkersCount], []).

process_response(Pid, { Url, Code, ResultUrls }) ->
    gen_server:cast(Pid, { process_response, { Url, Code, ResultUrls } }).


init([Url, WorkersCount]) ->
    LinksEts = ets:new(links, []),
    Workers = spawn_workers(WorkersCount),
    add_new_url(LinksEts, { Url, new, undefined }),

    State = #{
      base_url => Url,
      links => LinksEts,
      workers => Workers,
      workers_count => WorkersCount
     },

    gen_server:cast(self(), { start }),
    {ok, State}.

handle_call(_Msg, _From, State) ->
    { reply, ok, State }.


handle_cast({ start }, #{ links := Links, workers := Workers } = State) ->
    ok = process_urls(get_new_urls(Links), Workers, Links),
    { noreply, State };

handle_cast({ process_response, { Url, Code, ResultUrls } }, State) ->
    #{ links := Links, workers := Workers, base_url := BaseUrl } = State,
    case Code of
        200 ->
            set_url_state(Links, Url, processed),
            [add_new_url(Links, { X, new, Url }) || X <- ResultUrls ],

            ok = process_urls(get_new_urls(Links), Workers, Links);
        _ ->
            set_url_state(Links, Url, failed)
    end,
    case is_finished(Links) of
        true ->
            io:format("process finished ~p \n", [BaseUrl]);
            % io:format("ETS STATE: ~p \n", [ets:tab2list(Links)]);
        _ -> ok
    end,
    { noreply, State };

handle_cast(_Msg, State) -> { noreply, State }.
handle_info(_Msg, State) -> { noreply, State }.
terminate(_Msg, State) -> { noreply, State }.
code_change(_, _, _) -> ok.


%% PRIVATE

get_new_urls(Ets) ->
    MS = ets:fun2ms(fun({Url, UrlState, _} = Record) when UrlState == new ->
                            Record
                    end),
    ets:select(Ets, MS).

is_finished(Ets) ->
    MS = ets:fun2ms(fun({_, UrlState, _} = Record) when UrlState == new orelse UrlState == processing ->
                            Record
                    end),
    case ets:select(Ets, MS) of
        [] -> true;
        _ -> false
    end.

process_urls([], _Workers, _Ets) -> ok;
process_urls([{ Url, new, _ } | Urls], [Worker | OtherWorkers], Ets) ->
    send_to_worker(Url, Worker),
    set_url_state(Ets, Url, processing),
    process_urls(Urls, OtherWorkers ++ [Worker], Ets).

add_new_url(Ets, {Url, _State, _From } = Record) ->
    case ets:lookup(Ets, Url) of
        [] -> ets:insert(Ets, Record);
        _ -> ok
    end.

set_url_state(Ets, Url, State) ->
    ets:update_element(Ets, Url, [{2, State}]).

spawn_workers(Count) -> spawn_workers(Count, []).
spawn_workers(0, Acc) -> Acc;
spawn_workers(Count, Acc) ->
    {ok, WorkerPid } = crawler_worker_sup:start_child(self()),
    spawn_workers(Count - 1, [WorkerPid | Acc]).

send_to_worker(Url, Worker) ->
    crawler_worker:process_url(Worker, Url).
