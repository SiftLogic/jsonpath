
%% @copyright (c) 2012-2023 Gene Stevens.  All Rights Reserved.
%% @author Gene Stevens <gene@triplenexus.org>
%% @doc Fast javascript-like "path" notation for querying and updating JSON
%%
%% jsonpath - json data retrieval and updates via
%%            javascript-like notation
%%
%% This module provides a simple way to query and update JSON data using a path
%% notation similar to javascript.  The path notation is a string of property
%% names separated by dots.  For example, the path "menu.popup.menuitem[1]"
%% would return the second element of the array "menuitem" in the object.
%%
-module(jsonpath).
-export([add/3,
         delete/2,
         search/2,
         replace/3]).

-include_lib("kernel/include/logger.hrl").
%% -define(DBG(Fmt, Args), ?LOG_DEBUG(Fmt, Args)).

%% Copied from `jsx` excluding the with_tail option
-type json_term() :: [{binary() | atom(), json_term()}] | [{},...]
    | [json_term()] | []
    | #{ binary() | atom() => json_term() }
    | true | false | null
    | integer() | float()
    | binary() | atom()
    | calendar:datetime().

%% @doc Adds a new property to the JSON object at path, or adds a new element to array at path
-spec add(binary(), json_term(), json_term() | binary()) -> json_term().
add(Path, Value, Data) when is_binary(Data) ->
    add(Path, Value, json:decode(Data));
add(Path, Value, Data) when is_map(Data) ->
    handle_data(parse_path2(Path), {op_add, Value}, Data).

%% @doc Deletes a property from the JSON object at path, or deletes an element from array at path
-spec delete(binary(), json_term() | binary()) -> json_term().
delete(Path, Data) when is_binary(Data) ->
    delete(Path, json:decode(Data));
delete(Path, Data) when is_map(Data) ->
    handle_data(parse_path2(Path), op_delete, Data).

%% @doc Searches for a property in the JSON object at path, or searches for an element in array at path
-spec search(binary(), json_term() | binary()) -> any().
search(Path, Data) when is_binary(Data) ->
    search(Path, json:decode(Data));
search(Path, Data) when is_map(Data) ->
    search_data(parse_path2(Path), Data).

%% @doc Replaces a property in the JSON object at path,
%% or replaces an element in array at path.
%% If the replacement value is a function,
%% it will be called with the current value as the only argument
%% and the return value will be used as the replacement value.
-spec replace(binary(), json_term() | fun((json_term()) -> json_term()), json_term() | binary()) -> json_term().
replace(Path, Replace, Data) when is_binary(Data) ->
    replace(Path, Replace, json:decode(Data));
replace(Path, Fun, Data) when is_map(Data) andalso is_function(Fun, 1) ->
    handle_data(parse_path(Path), {op_transform, Fun}, Data);
replace(Path, Replace, Data) when is_map(Data) ->
    handle_data(parse_path(Path), {op_replace, Replace}, Data).

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
perform_op({op_transform, Fun}, Key, OldValue, Data) when is_map(Data) ->
    Data#{Key => Fun(OldValue)};
perform_op({op_transform, Fun}, _Key, OldValue, Data) when is_list(Data) ->
    [Fun(OldValue) | Data];
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

parse_path2(<<"$.", Path/binary>>) ->
    parse_path2_(Path, <<>>, []);
parse_path2(_) ->
    throw(invalid_path).

parse_path2_(<<>>, <<>>, Parts) ->
    lists:reverse(Parts);
parse_path2_(<<>>, Part, Parts) ->
    lists:reverse([Part | Parts]);
parse_path2_(<<$[, Rest/binary>>, _Part, Parts) ->
    parse_path2_(Rest, {array, <<>>}, Parts);
parse_path2_(<<$], Rest/binary>>, {array, Part}, Parts) ->
    parse_path2_(Rest, <<>>, [Part, Parts]);
parse_path2_(<<Char:1/binary, Rest/binary>>, {array, Part}, Parts) ->
    parse_path2_(Rest, {array, <<Part/binary, Char/binary>>}, Parts);
parse_path2_(<<"..", Rest/binary>>, Part, Parts) ->
    parse_path2_(Rest, {descendants, <<>>}, [Part | Parts]);
parse_path2_(<<Char:1/binary, Rest/binary>>, {descendants, Part}, Parts) ->
    parse_path2_(Rest, {descendants, <<Part/binary, Char/binary>>}, Parts);
parse_path2_(<<$., Rest/binary>>, Part, Parts) ->
    parse_path2_(Rest, <<>>, [Part | Parts]);
parse_path2_(<<$*, Rest/binary>>, _Part, Parts) ->
    parse_path2_(Rest, <<>>, [wildcard | Parts]).



