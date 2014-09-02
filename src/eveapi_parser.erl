-module(eveapi_parser).

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([xml_to_plist/1]).

%%%===================================================================
%%% API
%%%===================================================================

xml_to_plist(B) when is_binary(B) ->
    xml_to_plist(erlang:binary_to_list(B));
xml_to_plist(L) ->
    {XML, _} = xmerl_scan:string(L, [{space,normalize},{encoding,"utf-8"}]),
    process(XML).


%%%===================================================================
%%% Internal functions
%%%===================================================================

process(XML) when is_list(XML) ->
    lists:flatten([process(Element) || Element <- XML]);
process(#xmlElement{name = eveapi, content = Content}) ->
    case lists:keyfind(result, #xmlElement.name, filter_xml_elements(Content)) of
        false ->
            [];
        #xmlElement{} = El ->
            process(El)
    end;
process(#xmlElement{name = result, content = Content}) ->
    lists:flatten([process(El) || El <- filter_xml_elements(Content)]);
process(#xmlElement{name = rowset, attributes = Attrs, content = Content}) ->
    RowsetName =
        case lists:keyfind(name, #xmlAttribute.name, Attrs) of
            false ->
                unknown;
            #xmlAttribute{value = V} ->
                erlang:list_to_atom(V)
        end,
    {
      RowsetName,
      lists:filter(fun([]) -> false; (_) -> true end, [process(C) || C <- Content])
    };
process(#xmlElement{name = row, attributes = Attrs, content = Content}) ->
    lists:flatten([process(A) || A <- Attrs] ++ [process(lists:filter(fun(#xmlElement{}) -> true; (_) -> false end, Content))]);
process(#xmlElement{name = Name, attributes = [], content = [#xmlText{value = Text}]}) ->
    {Name, erlang:list_to_binary(Text)};
process(#xmlElement{name = Name, attributes = [], content = Elements}) ->
    {Name, [process(El) || El <- filter_xml_elements(Elements)]};
process(#xmlAttribute{name = Name, value = Value}) ->
    {Name, erlang:list_to_binary(Value)};
process(_) ->
    [].

filter_xml_elements(List) ->
    lists:filter(fun(#xmlElement{}) -> true; (_) -> false end, List).