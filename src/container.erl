-module(container).

-export([different_passwords/2]).

valid_password(Password) ->
    L = integer_to_list(Password),
    F = fun(I, {HasDouble, NeverDecrment, Last}) ->
                if Last =:= I -> {true, NeverDecrment, I};
                   Last > I -> {HasDouble, false, I};
                   true -> {HasDouble, NeverDecrment, I}
                end
        end,
    {Double, Decrement, _} = lists:foldl(F, {false, true, -1}, L),
    Double andalso Decrement.

valid_password2(Password) ->
    [H|T] = integer_to_list(Password),
    ValidDouble = fun(HasDouble, RunSize) ->
                          if RunSize =:= 2 -> true;
                             true -> HasDouble
                          end
                  end,
    F = fun(I, {HasDouble, NeverDecrment, RunSize, Last}) ->
                if Last =:= I -> {HasDouble, NeverDecrment, RunSize+1, I};
                   Last > I -> {HasDouble, HasDouble, false, 1, I};
                   true -> {ValidDouble(HasDouble, RunSize), NeverDecrment,
                            1, I}
                end
        end,
    {Double, Decrement, RunSize, _} = lists:foldl(F, {false, true, 1, H}, T),
    (Double orelse RunSize =:= 2) andalso Decrement.


different_passwords(Start, End) ->
    F = fun(X, {Valid1, Valid2}) ->
                case valid_password(X) of
                    true ->
                        case valid_password2(X) of
                            true -> {Valid1 + 1, Valid2 + 1};
                            false -> {Valid1 + 1, Valid2}
                        end;
                    false -> {Valid1, Valid2}
                end
        end,
    lists:foldl(F, {0, 0}, lists:seq(Start, End)).
