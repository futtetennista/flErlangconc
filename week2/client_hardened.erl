-module(client_hardened).
-export([start/1,init/2,stop/1]).


start(Params) ->
    Pid=spawn(client_hardened,init,[allocate,Params]),
    system ! {request,Pid,register},
    Pid.

init(Action,Params) ->
    process_flag(trap_exit,true),
    loop(Action,Params).

stop(Pid) ->
    self() ! {stop,Pid}.

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
    %% Use Pid this way clients won't fail if server failed
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
    after 10000 ->
            Msg
    end.

is_server_online() ->
    case whereis(frequency) of
        undefined ->
            ko;
        Pid ->
            {ok,Pid}
    end.
