-module(mn_climber_misc).

%% API exports
-export([
    ensure_ok/2,
    format/2,
    all_module_attributes/1,
    all_attr_modules/2,
    build_acyclic_graph/3,
    pid_change_node/2
]).

%%====================================================================
%% API functions
%%====================================================================

ensure_ok(ok, _) -> ok;
ensure_ok({error, Reason}, ErrorTag) -> throw({error, {ErrorTag, Reason}}).

format(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args)).

all_module_attributes(Name) ->
    Targets = loaded_applications_modules(),
    lists:foldl(
        fun({App, Module}, Acc) ->
            case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                N =:= Name]) of
                [] -> Acc;
                Atts -> [{App, Module, Atts} | Acc]
            end
        end, [], Targets).

all_attr_modules(Attr, Value) ->
    Targets = loaded_applications_modules(),
    [Module || {_App, Module} <- Targets,
        begin
            case catch Module:module_info(attributes) of
                {'EXIT', _} -> false;
                Attributes ->
                    lists:member({Attr, Value}, Attributes)
            end
        end].

build_acyclic_graph(VertexFun, EdgeFun, Graph) ->
    G = digraph:new([acyclic]),
    try
        [case digraph:vertex(G, Vertex) of
             false -> digraph:add_vertex(G, Vertex, Label);
             _ -> ok = throw({graph_error, {vertex, duplicate, Vertex}})
         end || GraphElem <- Graph,
            {Vertex, Label} <- VertexFun(GraphElem)],
        [case digraph:add_edge(G, From, To) of
             {error, E} -> throw({graph_error, {edge, E, From, To}});
             _ -> ok
         end || GraphElem <- Graph,
            {From, To} <- EdgeFun(GraphElem)],
        {ok, G}
    catch {graph_error, Reason} ->
        true = digraph:delete(G),
        {error, Reason}
    end.

pid_change_node(Pid, NewNode) ->
    {_OldNode, Cre, Id, Ser} = decompose_pid(Pid),
    compose_pid(NewNode, Cre, Id, Ser).

%%====================================================================
%% Internal functions
%%====================================================================

loaded_applications_modules() ->
    lists:usort(
        lists:append(
            [[{App, Module} || Module <- Modules] ||
                {App, _, _} <- application:loaded_applications(),
                begin
                    application:get_env(App, use_mn_climber, false)
                end,
                {ok, Modules} <- [application:get_key(App, modules)]])).

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} -> [];
        Attributes -> Attributes
    end.

decompose_pid(Pid) when is_pid(Pid) ->
    %% see http://erlang.org/doc/apps/erts/erl_ext_dist.html (8.10 and 8.7)
    Node = node(Pid),
    BinPid = term_to_binary(Pid),
    ByteSize = byte_size(BinPid),
    NodeByteSize = (ByteSize - 11),
    <<131, 103, _NodePrefix:NodeByteSize/binary, Id:32, Ser:32, Cre:8>> = BinPid,
    {Node, Cre, Id, Ser}.

compose_pid(Node, Cre, Id, Ser) ->
    <<131, NodeEnc/binary>> = term_to_binary(Node),
    binary_to_term(<<131, 103, NodeEnc/binary, Id:32, Ser:32, Cre:8>>).