%%
%% jsonpath - json data retrieval and updates via
%%            javascript-like notation
%%
%% Copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc Fast javascript-like "path" notation for querying and updating JSON
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
-module(jsonpath).
-export([search/2, replace/3]).

-include_lib("kernel/include/logger.hrl").
%% -define(DBG(Fmt, Args), ?LOG_DEBUG(Fmt, Args)).

search(Path, Data) when is_binary(Data) ->
    search(Path, jsx:decode(Data));
search(Path, Data) ->
    search_data(parse_path(Path), Data).

replace(Path, Replace, Data) when is_binary(Data) ->
    replace(Path, Replace, jsx:decode(Data));
replace(Path, Replace, Data) ->
    replace_data(parse_path(Path), Replace, Data).

replace_data([SearchHead|SearchTail], Replace, Structure) ->
    case Structure of
        Structure when is_map(Structure) ->
            %% ?DBG("map: ~p", [Structure]),
            replace_map([SearchHead|SearchTail], Replace, Structure);
        {TupleList} ->
            %% ?DBG("tuple list: ~p", [TupleList]),
            { replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) };
        List ->
            %% ?DBG("looks like a list: ~p", [List]),
            replace_list([SearchHead|SearchTail], Replace, List)
    end.

replace_list([SearchHead|SearchTail], Replace, List) ->
    try
        Index = list_to_integer(binary_to_list(SearchHead)) + 1,
        case (Index > length(List)) of
            true ->
                %% ?DBG("Index out of range ~p for list size ~p", [Index, length(List)]),
                undefined;
            false ->
                replace_list([Index|SearchTail], Replace, List, 1, [])
        end
    catch
        _:_ ->
            %% ?DBG("This is not an integer: ~p", [SearchHead]),
            undefined
    end.
replace_list([_SearchHead|_SearchTail], _Replace, [], _Count, Accum) ->
    %% ?DBG("at the end of this list with accum: ~p", [Accum]),
    lists:reverse(Accum);
replace_list([SearchHead|SearchTail], Replace, [Head|Tail], Count, Accum) ->
    %% ?DBG("list: ~p", [Head|Tail]),
    Data = case SearchHead of
               Count ->
                   %% ?DBG("Found index ~p", [Count]),
                   case SearchTail of
                       [] ->
                           Replace;
                       _SearchTail ->
                           %% ?DBG("Not last, so no replacement, but replaceing into: ~p", [Head]),
                           replace_data(SearchTail, Replace, Head)
                   end;
               _SearchHead ->
                   %% ?DBG("Not index ~p", [Count]),
                   Head
           end,
    replace_list([SearchHead|SearchTail], Replace, Tail, Count+1, [Data|Accum]).


replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) ->
    replace_tuple_list([SearchHead|SearchTail], Replace, TupleList, []).
replace_tuple_list([_SearchHead|_SearchTail], _Replace, [], Accum) ->
    %% ?DBG("at the end of this tuple list with accum: ~p", [Accum]),
    lists:reverse(Accum);
replace_tuple_list([SearchHead|SearchTail], Replace, [Head|Tail], Accum) ->
    %% ?DBG("tuple: ~p", [Head]),
    Data = case Head of
               {SearchHead, Value} ->
                   %% ?DBG("Found match for ~p: ~p", [SearchHead, {SearchHead, Value}]),
                   case SearchTail of
                       [] ->
                           {SearchHead, Replace};
                       _SearchTail ->
                           %% ?DBG("Not last, so no replacement, but replaceing into : ~p",[Head]),
                           {SearchHead, replace_data(SearchTail, Replace, Value) }
                   end;
               _Other ->
                   %% ?DBG("No match for ~p: ~p", [SearchHead, _Other]),
                   Head
           end,
    %% ?DBG("continue processing tail: ~p", [Tail]),
    replace_tuple_list([SearchHead|SearchTail], Replace, Tail, [Data|Accum]).

replace_map([SearchHead|SearchTail], Replace, Map) when is_map_key(SearchHead, Map) ->
    #{SearchHead := Value} = Map,
    %% ?DBG("Found match for ~p: ~p", [SearchHead, {SearchHead, Value}]),
    case SearchTail of
        [] ->
            Map#{SearchHead => Replace};
        _SearchTail ->
            %% ?DBG("Not last, so no replacement, but replacing",[]),
            Map#{SearchHead => replace_data(SearchTail, Replace, Value)}
    end;
replace_map(_, _Replace, Map) ->
    Map.

search_data([], Data) ->
    Data;
search_data([Head|Tail], Data) ->
    %% ?DBG("Searching for ~p in ~p", [Head,Data]),
    case Head of
        <<>> ->
            search_data(Tail, Data);
        _Other ->
            case Data of
                Data when is_map(Data) ->
                    %% ?DBG("found map: ~p", [Data]),
                    search_map([Head|Tail], Data);
                {_Tuple} ->
                    %% ?DBG("found tuple: ~p", [_Tuple]),
                    search_tuple([Head|Tail], Data);
                _List ->
                    %% ?DBG("found list: ~p", [_List]),
                    search_list([Head|Tail], Data)
            end
    end.

search_list([Head|Tail], List) ->
    %% ?DBG("list search for ~p in ~p",[Head, List]),
    try
        Index = list_to_integer(binary_to_list(Head)) + 1,
        case (Index > length(List)) of
            true ->
                undefined;
            false ->
                search_data(Tail, lists:nth(Index, List))
        end
    catch
        _:_ ->
            %% ?DBG("that wasn't an integer",[]),
            undefined
    end.

search_tuple([Head|Tail], Tuple) ->
    {TuplePayload} = Tuple,
    case lists:keyfind(Head, 1, TuplePayload) of
        false ->
            %% ?DBG("did not find tuple value for ~p. done.", [Head]),
            undefined;
        {Head,Value} ->
            %% ?DBG("found tuple value for ~p: ~p", [Head,Value]),
            search_data(Tail, Value)
    end.

search_map([Head|Tail], Map) when is_map_key(Head, Map) ->
    #{Head := Value} = Map,
    %% ?DBG("found map value for ~p: ~p", [Head,Value]),
    search_data(Tail, Value);
search_map([_Head | _Tail], _Map) ->
    %% ?DBG("did not find map value for ~p. done.", [_Head]),
    undefined.

parse_path(Path) ->
    Split = binary:split(Path, [<<".">>,<<"[">>,<<"]">>], [global]),
    lists:filter(fun(X) -> X =/= <<>> end, Split).
