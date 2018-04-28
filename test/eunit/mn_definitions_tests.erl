%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mn_definitions_tests).

-include_lib("eunit/include/eunit.hrl").
-record(mn_parent, {id, name, age, company, children}).
-record(children, {id, name, age, school}).

get_definitions_from_modules_test() ->
    meck:new(mn_climber_misc, [passthrough]),
    meck:expect(mn_climber_misc, all_attr_modules,
        fun(behavior, _) -> [mn_ct_table];
            (_, _) -> [] end),
    meck:new(mn_ct_table, [non_strict]),
    Definitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ],
    meck:expect(mn_ct_table, table_definitions, 0, Definitions),

    Definitions1 = mn_climber_table:get_definitions_from_modules(),

    meck:unload(mn_climber_misc),
    meck:unload(mn_ct_table),
    ?assertEqual(Definitions, Definitions1).

names_test() ->
    meck:new(mn_climber, [passthrough]),
    Definitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, Definitions),
    Names = mn_climber_table:names(),
    meck:unload(mn_climber),
    ?assertEqual([mn_parent], Names).