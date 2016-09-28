-module(helloworld).
-export([hello_world/1]).

hello_world(Name) -> io:fwrite("hello vai carai, ~s\n", [Name]).
