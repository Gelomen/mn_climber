%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mn_climber_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [
    empty_db_no_table,
    empty_db_create_table,
    table_conflict,
    auto_upgrade
    % todo 暂时测试还不通过
%%    rename_node_name
].

-record(mn_parent, {id, name, age, company, children}).
-record(children, {id, name, age, school}).

%%====================================================================
%% callback functions
%%====================================================================

init_per_testcase(TestCase, Config) ->
    set_mnesia_dir(Config, TestCase).

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% testcase functions
%%====================================================================

%% 空数据库,没有表
empty_db_no_table(_) ->
    {ok, Apps} = application:ensure_all_started(mn_climber),
    true = lists:member(mn_climber, Apps),

    stop_app(),
    ok.

%% 空数据库,建立表
empty_db_create_table(_) ->
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

    {ok, _Apps} = application:ensure_all_started(mn_climber),
    0 = mnesia:table_info(mn_parent, size),

    meck:unload(mn_climber),

    stop_app(),
    ok.

%% 表结构冲突
table_conflict(_) ->
    meck:new(mn_climber, [passthrough]),
    %旧结构
    OldDefinitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, OldDefinitions),

    % 启动
    {ok, _Apps} = application:ensure_all_started(mn_climber),

    % 插入错误的数据结构 Children=/=#children{_ = '_'}
    mnesia:dirty_write(#mn_parent{id = 1}),
    {error, {schema_integrity_check_failed, [
        {table_content_invalid, mn_parent,
            {mn_parent, '_', '_', '_', '_', {children, '_', '_', '_', '_'}},
            [{mn_parent, 1, undefined, undefined, undefined, undefined}]}
    ]}} = (catch mn_climber_mnesia:init()),

    mnesia:dirty_write(#mn_parent{id = 1, children = #children{id = 2}}),
    ok = (catch mn_climber_mnesia:init()),

    % 新结构
    NewDefinitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent) ++ [spouse]},
            {match, {mn_parent, '_', '_', '_', '_', #children{_ = '_'}, '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, NewDefinitions),

    % 检查
    {error, {schema_integrity_check_failed, [
        {table_attributes_mismatch, mn_parent,
            [id, name, age, company, children, spouse],
            [id, name, age, company, children]}
    ]}} = (catch mn_climber_mnesia:init()),

    % 转换为新结构
    mnesia:transform_table(mn_parent,
        fun({mn_parent, Id, Name, Age, Company, Children}) ->
            {mn_parent, Id, Name, Age, Company, Children, undefined}
        end, record_info(fields, mn_parent) ++ [spouse]),
    % 再检查一遍
    ok = (catch mn_climber_mnesia:init()),

    meck:unload(mn_climber),

    stop_app(),
    ok.

%% 自动更新表结构
auto_upgrade(_) ->
    meck:new(mn_climber, [passthrough]),
    meck:new(mn_climber_misc, [passthrough]),
    meck:new(mn_ct_upgrade, [non_strict]),
    meck:new(mn_ct_table, [non_strict]),

    %旧结构
    OldDefinitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, OldDefinitions),

    {ok, _Apps} = application:ensure_all_started(mn_climber),
    mnesia:dirty_write(#mn_parent{id = 1, children = #children{id = 2}}),
    % 关闭
    stop_app(),

    % 新结构
    NewDefinitions = [
        {mn_parent, [
            {disc_copies, [node()]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent) ++ [spouse]},
            {match, {mn_parent, '_', '_', '_', '_', #children{_ = '_'}, '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, NewDefinitions),

    % 转换为新结构
    meck:expect(mn_climber_misc, all_module_attributes, 1,
        [{test_app, mn_ct_upgrade, [{add_mn_parent_spouse, []}]}]),
    meck:expect(mn_ct_upgrade, add_mn_parent_spouse,
        fun() ->
            mnesia:transform_table(mn_parent,
                fun({mn_parent, Id, Name, Age, Company, Children}) ->
                    {mn_parent, Id, Name, Age, Company, Children, undefined}
                end, record_info(fields, mn_parent) ++ [spouse]),
            ok
        end),

    % 启动
    {ok, _} = application:ensure_all_started(mn_climber),

    % 关闭
    stop_app(),

    % 旧结构
    meck:expect(mn_climber, definitions, 0, OldDefinitions),

    % 转换为旧结构
    meck:expect(mn_climber_misc, all_module_attributes, 1,
        [{test_app, mn_ct_upgrade, [{del_mn_parent_spouse, []}]}]),
    meck:expect(mn_ct_upgrade, del_mn_parent_spouse,
        fun() ->
            mnesia:transform_table(mn_parent,
                fun({mn_parent, Id, Name, Age, Company, Children, _Spouse}) ->
                    {mn_parent, Id, Name, Age, Company, Children}
                end, record_info(fields, mn_parent)),
            ok
        end),

    % 启动
    {ok, _} = application:ensure_all_started(mn_climber),

    meck:unload(mn_climber),
    meck:unload(mn_climber_misc),

    stop_app(),
    ok.

rename_node_name(Config) ->
    OldNode = 'old_node_name@127.0.0.1',

    meck:new(mn_climber, [passthrough]),
    % 结构
    Definitions = [
        {mn_parent, [
            {disc_copies, [OldNode]},
            {record_name, mn_parent},
            {attributes, record_info(fields, mn_parent)},
            {match, #mn_parent{children = #children{_ = '_'}, _ = '_'}}
        ]}
    ],
    meck:expect(mn_climber, definitions, 0, Definitions),

    OldMnesiaDir0 = filename:join([?config(data_dir, Config), "Mnesia.old_node_name@127.0.0.1"]),
    ok = application:set_env(mnesia, dir, OldMnesiaDir0),
    MnesiaDir = filename:join([?config(priv_dir, Config), atom_to_list(?FUNCTION_NAME)]),
    mn_climber_mnesia:copy_db(MnesiaDir),
    ok = application:set_env(mnesia, dir, MnesiaDir),

    mn_climber_mnesia_rename:rename(node(), [{OldNode, node()}]),

    meck:unload(mn_climber),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

set_mnesia_dir(Config, TestCase) ->
    MnesiaDir = filename:join([?config(priv_dir, Config), atom_to_list(TestCase)]),
    ok = application:set_env(mnesia, dir, MnesiaDir),
    Config.

stop_app() ->
    ok = application:stop(mn_climber),
    ok = application:stop(mnesia),
    ok.