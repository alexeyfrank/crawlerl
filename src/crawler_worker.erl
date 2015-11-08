-module(crawler_worker).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/1]).
-export([process_url/2]).

process_url(Pid, Url) ->
    gen_server:cast(Pid, { process_url, Url }).

start_link(ControllerPid) ->
    gen_server:start_link(?MODULE, [ControllerPid], []).

init([ControllerPid]) ->
    State = #{ controller => ControllerPid },
    {ok, State}.

handle_cast({ process_url, Url }, #{ controller := ControllerPid } = State) ->
    case ibrowse:send_req(Url, [{ "Content-Type", "text/html" }], get) of
        { ok, Code , _Headers, Data } ->
            Urls = case Code of
                       "200" -> parse_response(unicode:characters_to_binary(Data), Url);
                       _ -> []
                   end,
            io:format("Process url ~p with code: ~p~n", [Url, Code]),
            % io:format("Process url ~p with code: ~p And Urls: ~p ~n", [Url, Code, Urls]),
            crawler_controller:process_response(ControllerPid, { Url, list_to_integer(Code), Urls });
        Rslt ->
            io:format("Request failed: ~p~n", [Rslt])
    end,
    { noreply, State };

handle_cast(_Msg, State) -> { noreply, State }.
handle_call(Msg, _From, State) -> { reply, Msg, State }.
handle_info(_Msg, State) -> { noreply, State }.
terminate(_Msg, State) -> { noreply, State }.
code_change(_, _, _) -> ok.


parse_response(Html, BaseUrl) ->
    Regex = "href=\"(.*?)\"",
    case re:run(Html, Regex, [ global, {capture, [1], list}]) of
        { match, MatchList } -> select_urls(MatchList, BaseUrl);
        _NoMatch ->
            io:format("no match ~n"),
            []
    end.

select_urls(MatchList, BaseUrl) ->
    { url, _Url, BaseHost, _Port, _, _, _, BaseScheme, _} = ibrowse_lib:parse_url(BaseUrl),
    lists:foldl(fun ([Url|_], Acc) ->
                        case convert_url(Url, BaseHost, BaseScheme) of
                            skip -> Acc;
                            NewUrl -> [NewUrl | Acc]
                        end
                end,
                [], MatchList).

convert_url([$/ | Tail], BaseHost, BaseScheme) ->
    Url = string:join([atom_to_list(BaseScheme), "://", BaseHost, "/", Tail], ""),
    { url, _, BaseHost, _, _, _, Path, _, _} = ibrowse_lib:parse_url(Url),
    [PathWithoutQuery | _] = string:tokens(Path, "?"),
    string:join([atom_to_list(BaseScheme), "://", BaseHost, PathWithoutQuery], "");

% convert_url([$? | _] = Url, BaseHost, BaseScheme) ->
%     string:join([atom_to_list(BaseScheme), "://", BaseHost, "/", Url], "");

convert_url([$h | _] = Url, BaseHost, _BaseScheme) ->
    { url, _, Host, _, _, _, Path, Scheme, _} = ibrowse_lib:parse_url(Url),
    [PathWithoutQuery | _] = string:tokens(Path, "?"),
    case Host of
        BaseHost -> string:join([atom_to_list(Scheme), "://", Host, PathWithoutQuery], "");
        _ -> skip
    end;

convert_url(_Url, _BaseHost, _BaseScheme) -> skip.
