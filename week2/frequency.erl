%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    Pid=spawn(?MODULE, init, []),
    register(?MODULE,Pid),
    Pid.

init() ->
    register(?MODULE,self()),
    process_flag(trap_exit, true),    %%% ADDED
    Frequencies = {get_frequencies(), []},
    io:format("[~w] Frequency server running~n",[self()]),
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
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
        {request,Pid,{restore,Freq}} ->
            {NewFrequencies,Reply}=restore(Frequencies,Pid,Freq),
            Pid ! {reply,Reply},
            loop(NewFrequencies);
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

restore(Freqs={[],_Allocated},_Pid,_Freq) ->
    {Freqs,{error,no_frequency}};
restore(Freqs={Free,Allocated},Pid,Freq) ->
    case lists:keysearch(Freq,1,Allocated) of
        false ->
            MonitorRef=monitor(process,Pid),
            NewFree=lists:filter(fun(X) -> X /= Freq end,Free),
            {{NewFree,[{Freq,Pid,MonitorRef}|Allocated]},{ok,Freq}};
        _ ->
            allocate(Freqs,Pid) %% Freq taken, assign a new one
    end.

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
