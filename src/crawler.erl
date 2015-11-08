-module(crawler).

-export([start/1, t/0]).

start(Url) ->
    ControllerPid = crawler_controller_sup:start_child(Url, 5),
    ControllerPid.

t() ->
    start("http://mdmstandart.ru/").
