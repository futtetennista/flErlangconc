%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0]).
-include_lib("eunit/include/eunit.hrl").

%% These are the start functions used to create and
%% initialize the server.

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies,Reply} = deallocate(Frequencies,Freq,Pid),
      Pid ! {reply,Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate_test() ->
    [
     ?assertEqual(allocate_twice(1),{{[11],[{10,1}]},{error,multiple_frequencies}})
    ].

allocate_twice(Pid) ->
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
     ?assertEqual(deallocate_unused(1),{{[11],[{10,1}]},{error,frequency_unused}})
    ].

deallocate_unused(Pid) ->
    InitialState={[10,11],[]},
    {NewState,_}=allocate(InitialState,Pid),
    deallocate(NewState,11,Pid).

deallocate(State={Free,Allocated},Freq,Pid) ->
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
    register(frequency,spawn(frequency,init,[])).
