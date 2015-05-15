-module(eveapi_parser_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("include/eveapi.hrl").

-define(XMLDIR, "priv/test/resources/xml/").

-define(ERROR_203_PLIST,
        {ok,{eveapi,[{version,2},
                     {currentTime,"2014-10-02 11:13:56"},
                     {error,[{code,"203"},{text,"Authentication failure."}]},
                     {cachedUntil,"2014-10-03 11:13:56"}]}}
       ).

-define(CHARCTER_SHEET_PLIST,
        {ok,{eveapi,
             [{version,1},
              {currentTime,"2007-06-18 22:49:01"},
              {result,
               [{characterID,"150337897"},
                {name,"corpslave"},
                {race,"Minmatar"},
                {'DoB',"2006-01-01 00:00:00"},
                {bloodLine,"Brutor"},
                {ancestry,"Slave Child"},
                {gender,"Female"},
                {corporationName,"corpexport Corp"},
                {corporationID,"150337746"},
                {allianceName,[]},
                {allianceID,"0"},
                {cloneName,"Clone Grade Pi"},
                {cloneSkillPoints,"54600000"},
                {balance,"190210393.87"},
                {attributeEnhancers,
                 [{intelligenceBonus,
                   [{augmentatorName,"Snake Delta"},
                    {augmentatorValue,"3"}]},
                  {memoryBonus,
                   [{augmentatorName,"Halo Beta"},
                    {augmentatorValue,"2"}]}]},
                {attributes,
                 [{intelligence,"6"},
                  {memory,"4"},
                  {charisma,"7"},
                  {perception,"12"},
                  {willpower,"10"}]},
                {skills,
                 [[{typeID,"3431"},{level,"3"},{skillpoints,"8000"}],
                  [{typeID,"3413"},{level,"3"},{skillpoints,"8000"}],
                  [{typeID,"21059"},{level,"1"},{skillpoints,"500"}],
                  [{typeID,"3416"},{level,"3"},{skillpoints,"8000"}],
                  [{typeID,"3445"},
                   {unpublished,"1"},
                   {skillpoints,"277578"}]]},
                {certificates,["1","5","19","239","282","32","258"]},
                {corporationRoles,[{roleID,"1"},{roleName,"roleDirector"}]},
                {corporationRolesAtHQ,[{roleID,"1"},{roleName,"roleDirector"}]},
                {corporationRolesAtBase,
                 [{roleID,"1"},{roleName,"roleDirector"}]},
                {corporationRolesAtOther,
                 [{roleID,"1"},{roleName,"roleDirector"}]},
                {corporationTitles,[{titleID,"1"},{titleName,"Member"}]}]},
              {cachedUntil,"2007-06-18 23:49:01"}]}}
       ).

-define(ERROR_203_MAP,
        {ok,#{eveapi => #{cachedUntil => "2014-10-03 11:13:56",
                          currentTime => "2014-10-02 11:13:56",
                          error => #{code => "203",text => "Authentication failure."},
                          version => 2}}}
       ).

-define(CHARCTER_SHEET_MAP,
        {ok,#{eveapi => #{cachedUntil => "2007-06-18 23:49:01",
                          currentTime => "2007-06-18 22:49:01",
                          result => #{'DoB' => "2006-01-01 00:00:00",
                                      allianceID => "0",
                                      allianceName => [],
                                      ancestry => "Slave Child",
                                      attributeEnhancers => #{intelligenceBonus => #{augmentatorName => "Snake Delta",augmentatorValue => "3"},
                                                              memoryBonus => #{augmentatorName => "Halo Beta",augmentatorValue => "2"}},
                                      attributes => #{charisma => "7",
                                                      intelligence => "6",
                                                      memory => "4",
                                                      perception => "12",
                                                      willpower => "10"},
                                      balance => "190210393.87",
                                      bloodLine => "Brutor",
                                      certificates => ["1","5","19","239","282","32","258"],
                                      characterID => "150337897",
                                      cloneName => "Clone Grade Pi",
                                      cloneSkillPoints => "54600000",
                                      corporationID => "150337746",
                                      corporationName => "corpexport Corp",
                                      corporationRoles => #{roleID => "1",roleName => "roleDirector"},
                                      corporationRolesAtBase => #{roleID => "1",roleName => "roleDirector"},
                                      corporationRolesAtHQ => #{roleID => "1",roleName => "roleDirector"},
                                      corporationRolesAtOther => #{roleID => "1",roleName => "roleDirector"},
                                      corporationTitles => #{titleID => "1",titleName => "Member"},
                                      gender => "Female",
                                      name => "corpslave",
                                      race => "Minmatar",
                                      skills => [#{level => "3",skillpoints => "8000",typeID => "3431"},
                                                 #{level => "3",skillpoints => "8000",typeID => "3413"},
                                                 #{level => "1",skillpoints => "500",typeID => "21059"},
                                                 #{level => "3",skillpoints => "8000",typeID => "3416"},
                                                 #{skillpoints => "277578",typeID => "3445",unpublished => "1"}]},
                          version => 1}}}
       ).

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
    ?assertEqual(?CHARCTER_SHEET_PLIST, eveapi_parser:encode(plist, B)),
    ?assertEqual(?CHARCTER_SHEET_MAP, eveapi_parser:encode(map, B)).

parser_error() ->
    {ok, B} = file:read_file(?XMLDIR ++ "Error/203.xml"),
    io:format("==========~n~p================~n~p================~n", [?ERROR_203_PLIST, eveapi_parser:encode(plist, B)]),
    ?assertEqual(?ERROR_203_PLIST, eveapi_parser:encode(plist, B)),
    ?assertEqual(?ERROR_203_MAP, eveapi_parser:encode(map, B)).

parse_xml({_Request, URL}) ->
    %%?debugFmt("r ~p u ~p path ~p~n", [_Request, URL, ?XMLDIR ++ filename:dirname(URL) ++ "/" ++ filename:basename(URL, ".aspx")]),
    {ok, B} = file:read_file(?XMLDIR ++ filename:dirname(URL) ++ "/" ++ filename:basename(URL, ".aspx")),
    eveapi_parser:encode(B),
    ok.
