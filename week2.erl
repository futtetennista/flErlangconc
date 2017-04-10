-module(week2).
-export([start_client/1,init_client/2,start_system/0,init_system/0,stop_system/0]).

%% CLIENT
start_client(Params) ->
    Pid=spawn(week2,init_client,[allocate,Params]),
    system ! {request,Pid,register},
    Pid.

init_client(Action,Params) ->
    process_flag(trap_exit,true),
    loop(Action,Params).

loop(Msg,Params) ->
    case is_server_online() of
        ko ->
            io:format("[~w] Server offline retrying...~n",[self()]),
            timer:sleep(7000),
            loop(allocate,Params);
        {ok,Pid} ->
            NewMsg=do_work(Pid,Msg),
            loop(NewMsg,Params)
    end.

do_work(Pid,Msg) ->
    Pid ! {request,self(),Msg},
    receive
        {reply,{ok,Freq}} ->
            io:format("[~w] Freq ~w allocated~n",[self(),Freq]),
            timer:sleep(5000),
            {deallocate,Freq};
        {reply,ok} ->
            io:format("[~w] Freq deallocated~n",[self()]),
            timer:sleep(1000),
            allocate;
        {stop,Pid} ->
            case Pid == whereis(system) of
                true ->
                    system ! ok;
                false ->
                    system ! {request,self(),unregister}
            end
    end.

is_server_online() ->
    case whereis(frequency) of
        undefined ->
            ko;
        Pid ->
            {ok,Pid}
    end.

%% SHOW STOPPER
%% Not so nice:
%% 1) it must be started before anything else
%% 2) if it's killed, server and clients will still run until and there will be no way to stop them (the text of the exercise doesn't actually specify how this scenario should be handled)
%% 3) when stopping the system normally, clients and servers must pattern-match on the system pid
start_system() ->
    register(system,spawn(week2,init_system,[])).

init_system() ->
    process_flag(trap_exit,true),
    loop([]).

stop_system() ->
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
            io:format("~w exited cause:~w~n",[Pid,Reason]),
            loop(lists:filter(fun(X) -> X /= Pid end,Pids));
        _ ->
            loop(Pids)
    end.

send_stop([]) ->
    done;
send_stop([Pid|Pids]) ->
    Pid ! {stop,system},
    receive
        ok ->
            send_stop(Pids)
    end.
