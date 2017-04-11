-module(frequency_supervisor).
-export([start/0,init/0,stop/0]).

start() ->
    register(?MODULE,spawn(?MODULE,init,[])).

init() ->
    process_flag(trap_exit,true),
    InitialState=empty,
    ServerPid=start_frequency_server(InitialState),
    supervise(ServerPid,InitialState).

start_frequency_server(State) ->
    spawn_link(frequency,init,[State]).

supervise(ServerPid,State) ->
    receive
        {state,NewState} ->
            supervise(ServerPid,NewState);
        stop ->
            ok;
        {'EXIT',Pid,_Reason} when Pid == ServerPid ->
            io:format("Restarting frequency server~n"),
            NewServerPid=start_frequency_server(State),
            supervise(NewServerPid,State);
        {'EXIT',Pid,normale} when Pid == self() ->
            io:format("Bye"),
            ok;
        {'EXIT',_Pid,Reason} ->
            io:format("Illegal exit reason: ~w~n",[Reason]),
            supervise(ServerPid,State)
    end.

stop() ->
    %% Why is this not working ?!
    %% exit(self(),normal).
    ?MODULE ! stop.
