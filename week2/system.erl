-module(system).
-export([start/0,init/0,stop/0]).

%% Not so nice:
%% - it must be started before anything else
%% - if it's killed, server and clients will still run until and there will be no way to stop them (the text of the exercise doesn't actually specify how this scenario should be handled)
%% - when stopping the system normally, clients and servers must pattern-match on the system pid (is this actually ugly in the erlang-world ?)

start() ->
    register(system,spawn(system,init,[])).

init() ->
    process_flag(trap_exit,true),
    loop([]).

stop() ->
    system ! {request,stop}.

%% Just keep looping waiting to be stopped somehow
loop(Pids) ->
    receive
        {request,stop} ->
            send_stop(Pids);
        {request,Pid,register} ->
            link(Pid),
            Pid ! ok,
            loop([Pid|Pids]);
        {request,Pid,unregister} ->
            unlink(Pid),
            Pid ! ok,
            loop(lists:filter(fun(X) -> X /= Pid end,Pids));
        {'EXIT',Pid,Reason} ->
            io:format("~w exited. Reason:~w~n",[Pid,Reason]),
            loop(lists:filter(fun(X) -> X /= Pid end,Pids));
        _ ->
            loop(Pids)
    end.

send_stop([]) ->
    ok;
send_stop([Pid|Pids]) ->
    Pid ! {stop,system},
    receive
        ok ->
            send_stop(Pids)
    end.
