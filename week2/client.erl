-module(client).
-export([start/1,init/2,stop/1]).

%% It' not clear to me what the clients should do if the server failed. The exercise says "we can’t expect the supervisor to restart the clients" so I guess we let them fail. Another way to handle the problem could be a supervisor only for clients?!

start(Params) ->
    spawn(?MODULE,init,[allocate,Params]).

init(Msg,Params) ->
    link(whereis(frequency_supervisor)),
    loop(Msg,Params).

stop(Pid) ->
    self() ! {stop,Pid}.

loop(Msg,Params) ->
    timer:sleep(1000),
    io:format("[~w] Sending req {~w}~n",[self(),Msg]),
    frequency ! {request,self(),Msg},
    receive
        {reply,{ko,frequency_not_allocated}} ->
            {_,Freq}=Msg,
            loop({restore,Freq},Params);
        {reply,{ok,Freq}} ->
            io:format("[~w] Freq ~w allocated~n",[self(),Freq]),
            %% timer:sleep(1000),
            loop({deallocate,Freq},Params);
        {reply,ok} ->
            io:format("[~w] Freq deallocated~n",[self()]),
            %% timer:sleep(1000),
            loop(allocate,Params)
    after 10000 ->
            loop(Msg,Params)
    end.
