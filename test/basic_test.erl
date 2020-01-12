-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    {ok, AllAtOnceHash} = erl_blake3:hash("foobarbaz"),
    ?assert(is_reference(AllAtOnceHash)),

    {ok, Hasher0} = erl_blake3:new(),
    ?assert(is_reference(Hasher0)),

    {ok, Hasher1} = erl_blake3:update(Hasher0, "foo"),
    ?assert(is_reference(Hasher1)),

    {ok, Hasher2} = erl_blake3:update(Hasher1, "bar"),
    ?assert(is_reference(Hasher2)),

    {ok, Hasher3} = erl_blake3:update(Hasher2, "baz"),
    ?assert(is_reference(Hasher3)),

    {ok, FinalHash} = erl_blake3:finalize(Hasher3),
    ?assert(is_reference(FinalHash)),

    ?assertEqual(erl_blake3:to_hex(AllAtOnceHash), erl_blake3:to_hex(FinalHash)).
