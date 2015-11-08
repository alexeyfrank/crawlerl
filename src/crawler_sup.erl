-module(crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupervisorSpecification = #{
      strategy => one_for_one,
      intensity => 10,
      period => 60},

    ChildSpecifications = [#{
      id => crawler_controller_sup,
      start => {crawler_controller_sup, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => supervisor,
      modules => [crawler_controller_sup]
     }, #{
      id => crawler_worker_sup,
      start => {crawler_worker_sup, start_link, []},
      restart => permanent,
      shutdown => 2000,
       type => supervisor,
      modules => [crawler_worker_sup]
     }],

    {ok, { SupervisorSpecification, ChildSpecifications } }.
