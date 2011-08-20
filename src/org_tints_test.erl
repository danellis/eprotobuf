-module(org_tints_test).
-export([descriptors/0]).

descriptors() -> [
    {<<"org.tints.test.Test">>, [
        {1, message, string, required},
        {2, number, int32, required},
        {3, letters, string, repeated},
        {4, greeting, <<"org.tints.test.Greeting">>, repeated}
    ]},
    {<<"org.tints.test.Greeting">>, [
        {1, salutation, string, required},
        {2, name, string, required}
    ]}
].
