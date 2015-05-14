%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2015, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created : 13 May 2015 by Roman Dayneko <roman.dayneko@gmail.com>
%%%
%%% thanks this article https://arifishaq.wordpress.com/2014/11/25/starting-to-play-with-xmerl/
%%%

-module(eveapi_parser).

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([encode/1, encode/2]).


%%%===================================================================
%%% API
%%%===================================================================

encode(B) when is_binary(B) ->
    encode(map, erlang:binary_to_list(B));
encode(L) when is_list(L) ->
    encode(map, L).

encode(Type, B) when is_binary(B) ->
    encode(Type, erlang:binary_to_list(B));
encode(plist, L) when is_list(L) ->
    {Result, _} = xmerl_scan:string(L, [{hook_fun, fun element_hook/2}, {acc_fun, fun acc_hook/3}, {space, normalize}, {encoding, "utf-8"}]),
    {ok, Result};
encode(map, L) when is_list(L) ->
    case encode(plist, L) of
        {ok, Result} ->
            {ok, plist_to_map([Result])};
        Error  ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

element_hook(#xmlElement{name = error, attributes = [#xmlAttribute{name = code, value = Code}], content = [Text]}, GlobalState) ->
    {{error, [{code, Code}, {text, Text}]}, GlobalState};
element_hook(#xmlElement{name = eveapi, content = Content, attributes = Attributes}, GlobalState) ->
    ApiVersion =
        case lists:keyfind(version, #xmlAttribute.name, Attributes) of
            false ->
                unknown;
            #xmlAttribute{value = V} ->
                erlang:list_to_integer(V)
        end,
    {{eveapi, [{version, ApiVersion} | Content]}, GlobalState};
element_hook(#xmlElement{name = rowset, content = Content, attributes = Attributes}, GlobalState) ->
    RowsetName =
        case lists:keyfind(name, #xmlAttribute.name, Attributes) of
            false ->
                unknown;
            #xmlAttribute{value = V} ->
                erlang:list_to_atom(V)
        end,
    {{RowsetName, Content}, GlobalState};
element_hook(#xmlElement{name = row, content = [], attributes = Attributes}, GlobalState) ->
    {[{Name, Value} || #xmlAttribute{name = Name, value = Value} <- Attributes], GlobalState};
element_hook(#xmlElement{name = Name, content = Content, attributes = []}, GlobalState) ->
    {{Name, Content}, GlobalState};
element_hook(#xmlText{value = Value}, GlobalState) ->
    {Value, GlobalState};
element_hook(Entity, GlobaState) ->
    {Entity, GlobaState}.

acc_hook(" ", Acc, GlobalState) ->
    {Acc, GlobalState};
acc_hook({certificates, Certificates}, Acc, GlobalState) ->
    {[{certificates, [Id || [{'certificateID', Id}] <- Certificates]} | Acc], GlobalState};
acc_hook({Key, [Value]}, Acc, GlobalState) when is_list(Value) ->
    {[{Key, Value} | Acc], GlobalState};
acc_hook(Entity, Acc, GlobalState) ->
    {[Entity | Acc], GlobalState}.

plist_to_map([]) ->
    [];
plist_to_map([H | _] = ListOfLists) when is_list(H) ->
    [plist_to_map(List) || List <- ListOfLists];
plist_to_map([{_, _} | _] = PList) ->
    maps:from_list([{K, plist_to_map(V)} || {K, V} <- PList]);
%%  #{Key => plist_to_map(Value) || Key := Value <- M};
%%  bugged maps: syntax error before: '||'
plist_to_map(Element) ->
    Element.
