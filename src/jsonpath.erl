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
-export([search/2,
         replace/3,
         add/3,
         delete/2]).

-include_lib("kernel/include/logger.hrl").
%% -define(DBG(Fmt, Args), ?LOG_DEBUG(Fmt, Args)).

search(Path, Data) when is_binary(Data) ->
    search(Path, jsx:decode(Data, [return_maps]));
search(Path, Data) when is_map(Data) ->
    search_data(parse_path(Path), Data).

replace(Path, Replace, Data) when is_binary(Data) ->
    replace(Path, Replace, jsx:decode(Data, [return_maps]));
replace(Path, Replace, Data) when is_map(Data) ->
    handle_data(parse_path(Path), {op_replace, Replace}, Data).

add(Path, Value, Data) when is_binary(Data) ->
    add(Path, Value, jsx:decode(Data, [return_maps]));
add(Path, Value, Data) when is_map(Data) ->
    handle_data(parse_path(Path), {op_add, Value}, Data).

delete(Path, Data) when is_binary(Data) ->
    delete(Path, jsx:decode(Data, [return_maps]));
delete(Path, Data) when is_map(Data) ->
    handle_data(parse_path(Path), op_delete, Data).

handle_data([SearchHead|SearchTail], Replace, Structure) ->
    case Structure of
        Structure when is_map(Structure) ->
            %% ?DBG("map: ~p", [Structure]),
            handle_map([SearchHead|SearchTail], Replace, Structure);
        List when is_list(List) ->
            %% ?DBG("looks like a list: ~p", [List]),
            replace_list([SearchHead|SearchTail], Replace, List)
    end.

replace_list([SearchHead|SearchTail], Replace, List) ->
    try
        %% Zero-index requires adding 1
        Index = binary_to_integer(SearchHead) + 1,
        case (Index > length(List)) of
            true ->
                %% ?DBG("Index out of range ~p for list size ~p", [Index, length(List)]),
                undefined;
            false ->
                handle_list([Index | SearchTail], Replace, List, 1, [])
        end
    catch
        _:_ ->
            %% ?DBG("This is not an integer: ~p", [SearchHead]),
            undefined
    end.

handle_list([_SearchHead | _SearchTail], _OpVal, [], _Count, Acc) ->
    lists:reverse(Acc);
handle_list([Count], OpVal, [Head | Tail], Count, Acc) ->
    handle_list([Count], OpVal, Tail, Count + 1, perform_op(OpVal, Count, Head, Acc));
handle_list([Count | SearchTail], OpVal, [Head | Tail], Count, Acc) ->
    NAcc = [handle_data(SearchTail, OpVal, Head) | Acc],
    handle_list([Count|SearchTail], OpVal, Tail, Count + 1, NAcc);
handle_list([SearchHead|SearchTail], OpVal, [Head | Tail], Count, Acc) ->
    handle_list([SearchHead|SearchTail], OpVal, Tail, Count + 1, [Head | Acc]).

handle_map([SearchHead|SearchTail], OpVal, Map) when is_map_key(SearchHead, Map) ->
    #{SearchHead := Value} = Map,
    %% ?DBG("Found match for ~p: ~p", [SearchHead, {SearchHead, Value}]),
    case SearchTail of
        [] ->
            perform_op(OpVal, SearchHead, Value, Map);
        _SearchTail ->
            %% ?DBG("Not last, so no replacement, but replacing",[]),
            Map#{SearchHead => handle_data(SearchTail, OpVal, Value)}
    end;
handle_map(_, _ReplaceValue, Map) ->
    Map.

perform_op({op_add, Value}, Key, OldValue, Data) when is_map(Data) andalso is_map(Value) ->
    Data#{Key => maps:merge(OldValue, Value)};
perform_op({op_add, Value}, _Key, [], Data) when is_list(Data) ->
    [Value | Data];
perform_op({op_add, Value}, _Key, OldValue, Data) when is_list(Data) ->
    [Value, OldValue | Data];
perform_op({op_replace, Value}, Key, _OldValue, Data) when is_map(Data) ->
    Data#{Key => Value};
perform_op({op_replace, Value}, _Key, _OldValue, Data) when is_list(Data) ->
    [Value | Data];
perform_op(op_delete, Key, _OldValue, Data) when is_map(Data) ->
    maps:remove(Key, Data);
perform_op(op_delete, _Key, _OldValue, Data) when is_list(Data) ->
    Data.

search_data([], Data) ->
    Data;
search_data([Head|Tail], Data) ->
    case Head of
        <<>> ->
            search_data(Tail, Data);
        _Other ->
            case Data of
                Data when is_map(Data) ->
                    %% ?DBG("found map: ~p", [Data]),
                    search_map([Head|Tail], Data);
                Data when is_list(Data) ->
                    %% ?DBG("found list: ~p", [Data]),
                    search_list([Head|Tail], Data);
                _ ->
                    undefined
            end
    end.

search_list([Head|Tail], List) ->
    %% ?DBG("list search for ~p in ~p",[Head, List]),
    try
        %% indexes are zero based, so add 1
        Index = binary_to_integer(Head) + 1,
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
