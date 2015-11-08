-module(crawler_controller_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Url, WorkersCount) ->
    supervisor:start_child(?SERVER, [Url, WorkersCount]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupervisorSpecification = #{
      strategy => simple_one_for_one,
      intensity => 10,
      period => 60},

    ChildSpecifications = [#{
      id => crawler_controller,
      start => {crawler_controller, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [crawler_controller]}],

    {ok, { SupervisorSpecification, ChildSpecifications } }.
