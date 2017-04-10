-module(week1).
-export([server/1,client/2,reverse_proxy/2,receiver1/0,receiver2/0,sorted_receiver/1]).


-spec palindrome(string()) -> boolean().
palindrome(Xs) ->
    Ys = to_lower(nopunct(Xs,[]),[]),
    Ys == lists:reverse(Ys,[]).

to_lower([],Acc) ->
    Acc;
to_lower([X|Xs],Acc) when X >= $A andalso X =< $Z ->
    XtoLowerCase = X + 32,
    to_lower(Xs,Acc ++ [XtoLowerCase]);
to_lower([X|Xs],Acc) ->
    to_lower(Xs,Acc ++ [X]).

%% skip character if is not a letter
nopunct([],Acc) ->
    Acc;
nopunct([X|Xs],Acc) ->
    case is_letter(X) of
        true ->
            nopunct(Xs,Acc++[X]);
        false ->
            nopunct(Xs,Acc)
    end.

is_letter(X) when (X >= $A andalso X =< $Z) orelse (X >= $a andalso X =< $z) ->
    true;
is_letter(_X) ->
    false.

server(Owner) ->
    receive
        {From,check,Str} ->
            handle_request(From,Str),
            server(Owner);
        {From,_msg} when From == Owner ->
            Owner ! "Shutting down"
    end.

handle_request(From,Str) ->
    case palindrome(Str) of
        true ->
            From ! {result,"'" ++ Str ++ "' is a palindrome",self()};
        false ->
            From ! {result,"'" ++ Str ++ "' is not a palindrome",self()}
    end.

client(Server,Caller) ->
    receive
        {check,Str} ->
            Server ! {self(),check,Str},
            client(Server,Caller);
        {result,Str,From} ->
            Caller ! {result,Str,From},
            client(Server,From);
        stop ->
            Caller ! "Here to serve you"
    end.

reverse_proxy(Owner,Servers) ->
    receive
        Msg={_Cid,check,_Str} ->
            pick_server(Servers) ! Msg,
            reverse_proxy(Owner,Servers);
        {Cid,_msg} when Cid == Owner ->
            Owner ! "Shutting down"
    end.

-spec pick_server([integer()]) -> integer().
pick_server(Servers) ->
    Index=random:uniform(length(Servers)),
    lists:nth(Index,Servers).


%% 1.8
receiver1() ->
    receive
        stop ->
            %% timer:sleep(1000),
            io:format("message:~s~n",[stop]);
        Msg ->
            %% timer:sleep(1000),
            io:format("message:~s~n",[Msg]),
            receiver1()
    end.

receiver2() ->
    receive
        Msg ->
            %% timer:sleep(1000),
            case Msg of
                stop ->
                    io:format("message:~s~n",[Msg]);
                _ ->
                    io:format("message:~s~n",[Msg]),
                    receiver2()
            end
    end.

sorted_receiver(State) ->
    receive
        {second,Str2} when State==first ->
            io:format("message2:~s~n",[Str2]),
            sorted_receiver(init);
        {first,Str1} ->
            io:format("message1:~s~n",[Str1]),
            sorted_receiver(first);
        stop ->
            io:format("message:~w~n",[stop]);
        _ ->
            sorted_receiver(State) %% consume the msg and loop
    end.
