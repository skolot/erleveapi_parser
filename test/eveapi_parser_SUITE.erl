-module(eveapi_parser_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/eveapi.hrl").

-define(XMLDIR, "priv/test/resources/xml/").

-ifdef(USE_PLIST).

-define(ERROR_203_PLIST, [{error,[{code,"203"},{text,"Authentication failure."}]}]).

-define(CHARCTER_SHEET_PLIST,
        [{characterID,<<"150337897">>},
         {name,<<"corpslave">>},
         {race,<<"Minmatar">>},
         {'DoB',<<"2006-01-01 00:00:00">>},
         {bloodLine,<<"Brutor">>},
         {ancestry,<<"Slave Child">>},
         {gender,<<"Female">>},
         {corporationName,<<"corpexport Corp">>},
         {corporationID,<<"150337746">>},
         {allianceName,[]},
         {allianceID,<<"0">>},
         {cloneName,<<"Clone Grade Pi">>},
         {cloneSkillPoints,<<"54600000">>},
         {balance,<<"190210393.87">>},
         {attributeEnhancers,
          [{intelligenceBonus,
            [{augmentatorName,<<"Snake Delta">>},{augmentatorValue,<<"3">>}]},
           {memoryBonus,
            [{augmentatorName,<<"Halo Beta">>},{augmentatorValue,<<"2">>}]}]},
         {attributes,
          [{intelligence,<<"6">>},
           {memory,<<"4">>},
           {charisma,<<"7">>},
           {perception,<<"12">>},
           {willpower,<<"10">>}]},
         {skills,
          [[{typeID,<<"3431">>},{level,<<"3">>},{skillpoints,<<"8000">>}],
           [{typeID,<<"3413">>},{level,<<"3">>},{skillpoints,<<"8000">>}],
           [{typeID,<<"21059">>},{level,<<"1">>},{skillpoints,<<"500">>}],
           [{typeID,<<"3416">>},{level,<<"3">>},{skillpoints,<<"8000">>}],
           [{typeID,<<"3445">>},{unpublished,<<"1">>},{skillpoints,<<"277578">>}]]},
         {certificates,
          [[{certificateID,<<"1">>}],
           [{certificateID,<<"5">>}],
           [{certificateID,<<"19">>}],
           [{certificateID,<<"239">>}],
           [{certificateID,<<"282">>}],
           [{certificateID,<<"32">>}],
           [{certificateID,<<"258">>}]]},
         {corporationRoles,[[{roleID,<<"1">>},{roleName,<<"roleDirector">>}]]},
         {corporationRolesAtHQ,[[{roleID,<<"1">>},{roleName,<<"roleDirector">>}]]},
         {corporationRolesAtBase,[[{roleID,<<"1">>},{roleName,<<"roleDirector">>}]]},
         {corporationRolesAtOther,[[{roleID,<<"1">>},{roleName,<<"roleDirector">>}]]},
         {corporationTitles,[[{titleID,<<"1">>},{titleName,<<"Member">>}]]}]
       ).
-else.

-define(ERROR_203_PLIST, [#{error => [#{code => "203"}, #{text => "Authentication failure."}]}]).

-define(CHARCTER_SHEET_PLIST,
        [#{characterID => <<"150337897">>},
         #{name => <<"corpslave">>},
         #{race => <<"Minmatar">>},
         #{'DoB' => <<"2006-01-01 00:00:00">>},
         #{bloodLine => <<"Brutor">>},
         #{ancestry => <<"Slave Child">>},
         #{gender => <<"Female">>},
         #{corporationName => <<"corpexport Corp">>},
         #{corporationID => <<"150337746">>},
         #{allianceName => []},
         #{allianceID => <<"0">>},
         #{cloneName => <<"Clone Grade Pi">>},
         #{cloneSkillPoints => <<"54600000">>},
         #{balance => <<"190210393.87">>},
         #{attributeEnhancers =>
               [#{intelligenceBonus =>
                      [#{augmentatorName => <<"Snake Delta">>},#{augmentatorValue => <<"3">>}]},
                #{memoryBonus =>
                  [#{augmentatorName => <<"Halo Beta">>}, #{augmentatorValue => <<"2">>}]}]},
         #{attributes =>
               [#{intelligence => <<"6">>},
                #{memory => <<"4">>},
                #{charisma => <<"7">>},
                #{perception => <<"12">>},
                #{willpower => <<"10">>}]},
         #{skills =>
               [[#{typeID => <<"3431">>}, #{level => <<"3">>}, #{skillpoints => <<"8000">>}],
                [#{typeID => <<"3413">>}, #{level => <<"3">>}, #{skillpoints => <<"8000">>}],
                [#{typeID => <<"21059">>}, #{level => <<"1">>}, #{skillpoints => <<"500">>}],
                [#{typeID => <<"3416">>}, #{level => <<"3">>}, #{skillpoints => <<"8000">>}],
                [#{typeID => <<"3445">>}, #{unpublished => <<"1">>}, #{skillpoints => <<"277578">>}]]},
         #{certificates =>
               [[#{certificateID => <<"1">>}],
                [#{certificateID => <<"5">>}],
                [#{certificateID => <<"19">>}],
                [#{certificateID => <<"239">>}],
                [#{certificateID => <<"282">>}],
                [#{certificateID => <<"32">>}],
                [#{certificateID => <<"258">>}]]},
         #{corporationRoles => [[#{roleID => <<"1">>}, #{roleName => <<"roleDirector">>}]]},
         #{corporationRolesAtHQ => [[#{roleID => <<"1">>}, #{roleName => <<"roleDirector">>}]]},
         #{corporationRolesAtBase => [[#{roleID => <<"1">>}, #{roleName => <<"roleDirector">>}]]},
         #{corporationRolesAtOther => [[#{roleID => <<"1">>}, #{roleName => <<"roleDirector">>}]]},
         #{corporationTitles => [[#{titleID => <<"1">>}, #{titleName => <<"Member">>}]]}]
       ).
-endif.

eveapi_parser_test_() ->
    [
     {
       "Parse all XML",
       [
        {
         erlang:atom_to_list(Type) ++ "/" ++ erlang:atom_to_list(Name),
          ?_test(parse_xml(R))
        }
        || {{Type, Name}, _} = R <- ?REQUESTS
       ]
     },
     {
       "parser equal test",
       ?_test(parser_equal())
     },
     {
       "parse error test",
       ?_test(parser_error())
     }
    ].

parser_equal() ->
    {ok, B} = file:read_file(?XMLDIR ++ "Char/CharacterSheet.xml"),
    ?assertEqual(?CHARCTER_SHEET_PLIST, eveapi_parser:xml_to_plist(B)).

parser_error() ->
    {ok, B} = file:read_file(?XMLDIR ++ "Error/203.xml"),
    ?assertEqual(?ERROR_203_PLIST, eveapi_parser:xml_to_plist(B)).

parse_xml({_Request, URL}) ->
    %%?debugFmt("r ~p u ~p path ~p~n", [_Request, URL, ?XMLDIR ++ filename:dirname(URL) ++ "/" ++ filename:basename(URL, ".aspx")]),
    {ok, B} = file:read_file(?XMLDIR ++ filename:dirname(URL) ++ "/" ++ filename:basename(URL, ".aspx")),
    eveapi_parser:xml_to_plist(B),
    ok.
