%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/1]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    Pid=spawn(?MODULE, init, []),
    register(?MODULE,Pid),
    Pid.

init(IntialFrequencies) ->
    register(?MODULE,self()),
    process_flag(trap_exit, true),
    Frequencies=get_frequencies(IntialFrequencies),
    io:format("[~w] Frequency server running~n",[self()]),
    loop(Frequencies).

% Hard Coded
get_frequencies(empty) ->
    {[10,11,12,13,14,15],[]};
get_frequencies(Frequencies) ->
    Frequencies.

%% The Main Loop

loop(Frequencies) ->
    frequency_supervisor ! {state,Frequencies},
    receive
        {request,Pid,allocate} ->
            {NewFrequencies,Reply}=allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request,Pid,{deallocate,Freq}} ->
            case deallocate(Frequencies, Freq) of
                {ko,Reason} ->
                    Pid ! {reply,{ko,Reason}},
                    loop(Frequencies);
                NewFrequencies ->
                    Pid ! {reply,ok},
                    loop(NewFrequencies)
                end;
        %% {request,Pid,{restore,Freq}} ->
        %%     {NewFrequencies,Reply}=restore(Frequencies,Pid,Freq),
        %%     Pid ! {reply,Reply},
        %%     loop(NewFrequencies);
        {'EXIT', Pid, _Reason} ->
            io:format("~w exited~n",[Pid]),
            NewFrequencies=exited(Frequencies,Pid),
            case NewFrequencies == Frequencies of
                false ->
                    loop(NewFrequencies);
                true ->
                    stop %% Supervisor died
            end
  end.

%% Functional interface

allocate() ->
    ?MODULE ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} -> Reply
    end.

stop() ->
    ?MODULE ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[],Allocated},_Pid) ->
    {{[], Allocated}, {error,no_frequency}};
allocate({[Freq|Free],Allocated},Pid) ->
    MonitorRef=monitor(process,Pid),
    {{Free, [{Freq,Pid,MonitorRef}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    case lists:keysearch(Freq,1,Allocated) of
        {value,{Freq,_Pid,MonitorRef}} ->
            demonitor(MonitorRef),
            NewAllocated=lists:keydelete(Freq, 1, Allocated),
            {[Freq|Free],NewAllocated};
        false ->
            {ko,frequency_not_allocated}
    end.

exited({Free, Allocated},Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,_Pid,MonitorRef}} ->
            demonitor(MonitorRef),
            NewAllocated=lists:keydelete(Freq,1,Allocated),
            {[Freq|Free],NewAllocated};
        false ->
            {Free,Allocated}
    end.
