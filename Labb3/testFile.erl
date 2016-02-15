-module(testFile).
-export([add/2, hello/0, greet_and_add_two/1]).

add(A,B) ->
	A + B.

hello() ->
	io:format("hello!~n").

greet_and_add_two(X) ->
	hello(),
	add(X,2).