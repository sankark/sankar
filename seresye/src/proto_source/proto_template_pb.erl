-file("src/proto_template_pb.erl", 1).

-module(proto_template_pb).

-export([encode_node/1, decode_node/1,
	 encode_clientstate/1, decode_clientstate/1,
	 encode_knowledgebase/1, decode_knowledgebase/1,
	 encode_response/1, decode_response/1, encode_request/1,
	 decode_request/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2]).

-record(node, {node_id, cpu, heap}).

-record(clientstate, {action}).

-record(knowledgebase, {nodes}).

-record(response, {kb, cs}).

-record(request, {req_name, kb}).

encode(Record) -> encode(element(1, Record), Record).

encode_node(Record) when is_record(Record, node) ->
    encode(node, Record).

encode_clientstate(Record)
    when is_record(Record, clientstate) ->
    encode(clientstate, Record).

encode_knowledgebase(Record)
    when is_record(Record, knowledgebase) ->
    encode(knowledgebase, Record).

encode_response(Record)
    when is_record(Record, response) ->
    encode(response, Record).

encode_request(Record)
    when is_record(Record, request) ->
    encode(request, Record).

encode(request, Record) ->
    iolist_to_binary(iolist(request, Record) ++
		       encode_extensions(Record));
encode(response, Record) ->
    iolist_to_binary(iolist(response, Record) ++
		       encode_extensions(Record));
encode(knowledgebase, Record) ->
    iolist_to_binary(iolist(knowledgebase, Record) ++
		       encode_extensions(Record));
encode(clientstate, Record) ->
    iolist_to_binary(iolist(clientstate, Record) ++
		       encode_extensions(Record));
encode(node, Record) ->
    iolist_to_binary(iolist(node, Record) ++
		       encode_extensions(Record)).

encode_extensions(_) -> [].

iolist(request, Record) ->
    [pack(1, optional,
	  with_default(Record#request.req_name, "nodes"), string,
	  []),
     pack(2, required, with_default(Record#request.kb, none),
	  knowledgebase, [])];
iolist(response, Record) ->
    [pack(1, required,
	  with_default(Record#response.kb, none), knowledgebase,
	  []),
     pack(2, required,
	  with_default(Record#response.cs, none), clientstate,
	  [])];
iolist(knowledgebase, Record) ->
    [pack(1, repeated,
	  with_default(Record#knowledgebase.nodes, none), node,
	  [])];
iolist(clientstate, Record) ->
    [pack(1, repeated,
	  with_default(Record#clientstate.action, none), string,
	  [])];
iolist(node, Record) ->
    [pack(1, required,
	  with_default(Record#node.node_id, none), string, []),
     pack(2, optional, with_default(Record#node.cpu, none),
	  int32, []),
     pack(3, optional, with_default(Record#node.heap, none),
	  int32, [])].

with_default(Val, none) -> Val;
with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(pikachu, value) -> 1.

int_to_enum(_, Val) -> Val.

decode_node(Bytes) when is_binary(Bytes) ->
    decode(node, Bytes).

decode_clientstate(Bytes) when is_binary(Bytes) ->
    decode(clientstate, Bytes).

decode_knowledgebase(Bytes) when is_binary(Bytes) ->
    decode(knowledgebase, Bytes).

decode_response(Bytes) when is_binary(Bytes) ->
    decode(response, Bytes).

decode_request(Bytes) when is_binary(Bytes) ->
    decode(request, Bytes).

decode(enummsg_values, 1) -> value1;
decode(request, Bytes) when is_binary(Bytes) ->
    Types = [{2, kb, knowledgebase, [is_record]},
	     {1, req_name, string, []}],
    Defaults = [{1, req_name, "nodes"}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(request, Decoded);
decode(response, Bytes) when is_binary(Bytes) ->
    Types = [{2, cs, clientstate, [is_record]},
	     {1, kb, knowledgebase, [is_record]}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(response, Decoded);
decode(knowledgebase, Bytes) when is_binary(Bytes) ->
    Types = [{1, nodes, node, [is_record, repeated]}],
    Defaults = [{1, nodes, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(knowledgebase, Decoded);
decode(clientstate, Bytes) when is_binary(Bytes) ->
    Types = [{1, action, string, [repeated]}],
    Defaults = [{1, action, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(clientstate, Decoded);
decode(node, Bytes) when is_binary(Bytes) ->
    Types = [{3, heap, int32, []}, {2, cpu, int32, []},
	     {1, node_id, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(node, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keysearch(FNum, 1, Types) of
      {value, {FNum, Name, Type, Opts}} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal =
				      decode(list_to_atom(string:to_lower(atom_to_list(Type))),
					     V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([int_to_enum(Type, Value1)
					      | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keysearch('$extensions', 2, Acc) of
	    {value, {_, _, Dict}} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(request, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, request),
						   Record, Name, Val)
			  end,
			  #request{}, DecodedTuples),
    Record1;
to_record(response, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       response),
						   Record, Name, Val)
			  end,
			  #response{}, DecodedTuples),
    Record1;
to_record(knowledgebase, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       knowledgebase),
						   Record, Name, Val)
			  end,
			  #knowledgebase{}, DecodedTuples),
    Record1;
to_record(clientstate, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       clientstate),
						   Record, Name, Val)
			  end,
			  #clientstate{}, DecodedTuples),
    Record1;
to_record(node, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, node),
						   Record, Name, Val)
			  end,
			  #node{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keysearch(Fnum, 1, Types) of
	       {value, {Fnum, Name, Type, Opts}} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal =
					       decode(list_to_atom(string:to_lower(atom_to_list(Type))),
						      V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

