-module(lysefgg).

%% should of course be replaced by
%% -export in reality :-) e.g.:
%% -export([greet/2]).
-compile(export_all).

hello() -> io:format("Hello, world!~n").

%% pattern matching
greet(male, Name) -> io:format("Hello, Mr. ~s ~n", [Name]);
greet(female, Name) -> io:format("Hello, Mrs. ~s ~n", [Name]);
greet(_, Name) -> io:format("Hello ~s ~n", [Name]).

canDrinkAlcohol(Age) when Age >= 18 -> true;
canDrinkAlcohol(_) -> false.

insert(X, []) -> [X];
insert(X, Set) ->
  case lists:member(X, Set) of
    true -> Set;
    false -> [X|Set]
  end.

%% Types
myAnd(_, false) -> false;
myAnd(false, _) -> false;
myAnd(_, _) -> true.

%% Recursion
fac(N) when N == 0 -> 1;
fac(N) -> fac(N - 1) * N.

len([]) -> 0;
len([_|T]) -> 1 + len(T).

%% Lambdas
myAdder(X,Y) -> X() + Y().

