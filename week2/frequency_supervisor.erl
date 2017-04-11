-module(frequency_supervisor).
-export([start/0,init/0,stop/0]).

start() ->
    register(?MODULE,spawn(frequency_supervisor,init,[])).

init() ->
    process_flag(trap_exit,true),
    ServerPid=start_frequency_server(),
    supervise(ServerPid).

start_frequency_server() ->
    spawn_link(frequency,init,[]).

supervise(ServerPid) ->
    receive
        stop ->
            ok;
        {'EXIT',Pid,_Reason} when Pid == ServerPid ->
            io:format("Restarting frequency server~n"),
            NewServerPid=start_frequency_server(),
            supervise(NewServerPid);
        {'EXIT',Pid,normal} when Pid == self() ->
            ok;
        {'EXIT',Pid,Reason} ->
            io:format("Illegal exit reason: ~w~n",[Reason]),
            supervise(ServerPid)
    end.

stop() ->
    %% Why is this not working => exit(self(),normal).
    ?MODULE ! stop.
