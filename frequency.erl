%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/1,start/0,start_debug/1,stop/0,allocate/1,deallocate/2]).
-include_lib("eunit/include/eunit.hrl").

%% These are the start functions used to create and
%% initialize the server.

init(TestLatencyMs) ->
  Frequencies={get_frequencies(),[]},
  loop({Frequencies,erlang:system_time(millisecond)},TestLatencyMs).

% Hard Coded
get_frequencies() ->
    [10,11,12,13,14,15].

%% The Main Loop

%% loop_test() ->
    %% [
    %%  ?assertEqual()
    %% ].


loop(State,TestLatencyMs) ->
    {Frequencies,LastClearTimestampMs}=State,
    timer:sleep(TestLatencyMs),
    receive
        {request,Pid,Req,{timeout,TimeoutMs}} ->
            case request_timed_out(TimeoutMs) of
                false ->
                    NewFrequencies=process_request(Frequencies,{request,Pid,Req}),
                    ClearTimestampMs=consider_clear(LastClearTimestampMs),
                    loop({NewFrequencies,ClearTimestampMs},TestLatencyMs);
                true ->
                    ClearTimestampMs=consider_clear(LastClearTimestampMs),
                    loop({Frequencies,ClearTimestampMs},TestLatencyMs)
            end;
        {request, Pid, stop} ->
            Pid ! {reply,self(),stopped}
    after 2000 ->
            clear(),
            loop({Frequencies,erlang:system_time(millisecond)},TestLatencyMs)
    end.

consider_clear(LastClearTimestampMs) ->
    case (LastClearTimestampMs + 2000) >= erlang:system_time(millisecond) of
        true ->
            clear(),
            erlang:system_time(millisecond);
        false ->
            LastClearTimestampMs
    end.

request_timed_out(TimeoutMs) ->
    erlang:system_time(millisecond) >= TimeoutMs.

process_request(Frequencies,{request,Pid,allocate}) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    Pid ! {reply,self(),Reply},
    NewFrequencies;
process_request(Frequencies,{request,Pid,{deallocate, Freq}}) ->
    {NewFrequencies,Reply} = deallocate(Frequencies,Pid,Freq),
    Pid ! {reply,self(),Reply},
    NewFrequencies.

clear() ->
    receive
        Msg ->
            io:format("Clearing msg: ~w~n",[Msg]),
            clear()
    after 0 ->
            clear_ok
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate_test() ->
    [
     ?assertEqual(test_allocate_twice(1),{{[11],[{10,1}]},{error,multiple_frequencies}})
    ].

test_allocate_twice(Pid) ->
    InitialState={[10,11],[]},
    {NewState,_}=allocate(InitialState,Pid),
    allocate(NewState,Pid).

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate(State={[Freq|Free], Allocated}, Pid) ->
    case lists:keyfind(Pid,2,Allocated) of
        false ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        _ ->
            {State,{error,multiple_frequencies}}
    end.

deallocate_test() ->
    [
     ?assertEqual(deallocate({[10,11],[]},10,1),{{[10,11],[]},{error,frequency_unused}}),
     ?assertEqual(test_deallocate_unused(1),{{[11],[{10,1}]},{error,frequency_unused}})
    ].

test_deallocate_unused(Pid) ->
    InitialState={[10,11],[]},
    {NewState,_}=allocate(InitialState,Pid),
    deallocate(NewState,Pid,11).

deallocate(State={Free,Allocated},Pid,Freq) ->
    case lists:keyfind(Freq,1,Allocated) of
        false ->
            {State,{error,frequency_unused}};
        {_,OwnerPid} when Pid/=OwnerPid ->
            {State,{error,frequency_unused}};
        _ ->
            NewAllocated=lists:keydelete(Freq,1,Allocated),
            {{[Freq|Free],NewAllocated},ok}
    end.

start() ->
    register(frequency,spawn(frequency,init,[0])).

start_debug(TestLatencyMs) ->
    register(frequency,spawn(frequency,init,[TestLatencyMs])).

stop() ->
    frequency ! {request,self(),stop},
    unregister(frequency).

allocate(Pid) ->
    frequency ! {request,Pid,allocate,{timeout,erlang:system_time(millisecond)+500}}.

deallocate(Pid,Freq) ->
    frequency ! {request,Pid,{deallocate,Freq},{timeout,erlang:system_time(millisecond)+500}}.
