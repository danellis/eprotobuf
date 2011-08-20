-module(pbtest).
-export([test/0]).

test() ->
    protobuf:load(org_tints_test),
    {ok, Data} = file:read_file('test.pb'),
    protobuf:decode(<<"org.tints.test.Test">>, Data).
