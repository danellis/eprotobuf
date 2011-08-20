-module(protobuf).
-export([load/1, decode/2]).
-define(VARINT, 0).
-define(INT64, 1).
-define(STRING, 2).

load(Module) ->
    store_descriptors(Module:descriptors()).

store_descriptors(Descriptors) ->
    case Descriptors of
        [] -> ok;

        [{Name, Fields} | Tail] ->
            store_fields(Name, Fields),
            store_descriptors(Tail)
    end.

store_fields(Message, Fields) ->
    case Fields of
        [] -> ok;
        
        [{Tag, Name, Type, Cardinality} | Tail] ->
            put({Message, Name}, {Tag, Type, Cardinality}),
            put({Message, Tag}, {Name, Type, Cardinality}),
            store_fields(Message, Tail)
    end.

decode(MessageName, Data) ->
    io:format("Decoding ~p~n", [MessageName]),
    decode_message(MessageName, dict:new(), Data).
    
decode_message(MessageName, Message, Data) ->
    case Data of
        <<>> ->
            Message;
        <<_/bytes>> ->
            {Tag, Value, Rest} = next_field(Data),
            {FieldName, FieldType, Cardinality} = get({MessageName, Tag}),
            NewMessage = case FieldType of
                <<_/bytes>> -> store_field(Message, FieldName, decode(FieldType, Value), Cardinality);
                _ -> store_field(Message, FieldName, Value, Cardinality)
            end,
            decode_message(MessageName, NewMessage, Rest)
    end.

next_field(Data) ->
    {Key, ValueAndRest} = decode_value(?VARINT, Data),
    WireType = Key band 2#111,
    Tag = Key bsr 3,
    {Value, Rest} = decode_value(WireType, ValueAndRest),
    {Tag, Value, Rest}.

decode_value(WireType, Data) when WireType == ?VARINT ->
    case Data of
        <<1:1, Value:7, Rest/bytes>> ->
            {V, R} = decode_value(?VARINT, Rest),
            {Value + (V bsl 7), R};
        <<0:1, Value:7, Rest/bytes>> ->
            {Value, Rest}
    end;

decode_value(WireType, Data) when WireType == ?INT64 ->
    <<Value:64, Rest/bytes>> = Data,
    {Value, Rest};

decode_value(WireType, Data) when WireType == ?STRING ->
    {Length, ValueAndRest} = decode_value(?VARINT, Data),
    <<Value:Length/bytes, Rest/bytes>> = ValueAndRest,
    {Value, Rest}.

store_field(Message, FieldName, Value, Cardinality) ->
    case Cardinality of
        required ->
            dict:store(FieldName, Value, Message);

        _ when is_atom(repeated); is_atom(optional) ->
            dict:append(FieldName, Value, Message)
    end.
