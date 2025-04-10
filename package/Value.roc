## JSON is a human-readable data interchange format.
##
## This module provides functions for encoding and decoding JSON data, using Elm-style
## "decoders" and "encoders". This uses `lukewilliamboswell/roc-json` for JSON parsing
## and serialization. Compared to using that module directly, this module provides better
## support for variably-shaped data.
##
## Here is an example where the "id" field might be either a string or a number:
##
## ```
## MsgId : [ StringId Str, NumberId U64 ]
##
## json_str = """
## {
##   "id": 123,
##   "foo": "Bar",
##   "baz": 42
## }
## """
## msg_id_recipe = field("id", one_of [ map(string, StringId), map(number, NumberId) ])
##
## name = decode_str(json_str, msg_id_recipe)
## expect name == Ok (NumberId 123)
## ```
module [
    Value,
    json_decoder,
    decode_str,
    decode_bytes,
    decode_val,
    Error,
    Recipe,
    string,
    bool,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,
    dec,
    list,
    dict,
    key_value_pairs,
    build_record,
    field,
    at,
    index,
    optional_field,
    nullable_field,
    optional_nullable_field,
    one_of,
    map,
    map2,
    map3,
    map4,
    map5,
    map6,
    map7,
    map8,
    value,
    null,
    succeed,
    fail,
    and_then,
    from_string,
    from_bool,
    from_i8,
    from_i16,
    from_i32,
    from_i64,
    from_u8,
    from_u16,
    from_u32,
    from_u64,
    from_dec,
    null_val,
    from_list,
    from_list_val,
    from_object,
    from_dict,
]

import json.Json

# Decoding

## An opaque type that represents a JSON value.
## This has been deserialized and confirmed to be valid JSON.
Value := [
    Null,
    String Str,
    Boolean Bool,
    Number Dec,
    Array (List Value),
    Object (Dict Str Value),
]
    implements [
        Eq,
        Decoding { decoder: json_decoder },
    ]

json_decoder : Decoder Value Json.Json
json_decoder = Decode.custom |bytes, fmt|
    when bytes is
        [] -> { result: Err(TooShort), rest: [] }
        ['n', 'u', 'l', 'l', .. as rest] -> { result: Ok(@Value(Null)), rest: rest }
        ['"', ..] -> Decode.decode_with bytes Decode.string fmt |> Decode.map_result |s| @Value(String s)
        ['t', 'r', 'u', 'e', .. as rest] -> { result: Ok(@Value(Boolean Bool.true)), rest: rest }
        ['f', 'a', 'l', 's', 'e', .. as rest] -> { result: Ok(@Value(Boolean Bool.false)), rest: rest }
        [c, ..] if (c >= '0' and c <= '9') or c == '-' -> Decode.decode_with bytes Decode.dec fmt |> Decode.map_result |i| @Value(Number i)
        ['[', ..] -> Decode.decode_with bytes (Decode.list json_decoder) fmt |> Decode.map_result |a| @Value(Array a)
        ['{', ..] ->
            Decode.decode_with
                bytes
                (
                    Decode.record
                        (Dict.empty {})
                        (|so_far, label|
                            Keep
                                (
                                    Decode.custom |inner_bytes, innerFmt|
                                        Decode.decode_with inner_bytes json_decoder innerFmt
                                        |> Decode.map_result |json| Dict.insert so_far label json
                                ))
                        (|decoded_dict, _fmt| Ok(@Value(Object decoded_dict)))
                )
                fmt

        _ -> { result: Err(TooShort), rest: bytes }

## Applies a recipe to a string. This parses the string into a `Value` and then applies the
## recipe to the resulting value.
decode_str : Str, Recipe output -> Result output Error
decode_str = |text, @Recipe recipe|
    adt = Decode.from_bytes(Str.to_utf8(text), Json.utf8_with({ null_decode_as_empty: Bool.false })) ? ParseErr
    recipe(adt) |> Result.map_err(RecipeErr)

expect
    actual = decode_str("\"test\"", string)
    actual == Ok("test")
expect
    actual = decode_str("42", dec)
    actual == Ok(42)
expect
    actual = decode_str("true", bool)
    actual == Ok(Bool.true)
expect
    actual = decode_str("null", null(ValueWasNull))
    actual == Ok(ValueWasNull)

## Applies a recipe to utf-8 bytes. This parses the bytes into a `Value` and then applies the
## recipe to the resulting value.
decode_bytes : List U8, Recipe output -> Result output Error
decode_bytes = |bytes, @Recipe recipe|
    adt = Decode.from_bytes(bytes, Json.utf8_with({ null_decode_as_empty: Bool.false })) ? ParseErr
    recipe(adt) |> Result.map_err(RecipeErr)

expect
    actual = decode_bytes([0x22, 0x74, 0x65, 0x73, 0x74, 0x22], string)
    actual == Ok("test")
expect
    actual = decode_bytes([0x34, 0x32], dec)
    actual == Ok(42)
expect
    actual = decode_bytes([0x74, 0x72, 0x75, 0x65], bool)
    actual == Ok(Bool.true)
expect
    actual = decode_bytes([0x6e, 0x75, 0x6c, 0x6c], null(ValueWasNull))
    actual == Ok(ValueWasNull)

## Applies a recipe to a value. This value may have been left over from a previous decoding.
decode_val : Value, Recipe output -> Result output Error
decode_val = |val, @Recipe recipe|
    recipe(val) |> Result.map_err(RecipeErr)

expect
    actual = decode_val(Value.from_string("test"), string)
    actual == Ok("test")
expect
    actual = decode_val(Value.from_dec(42), dec)
    actual == Ok(42)
expect
    actual = decode_val(Value.from_bool(Bool.true), bool)
    actual == Ok(Bool.true)
expect
    actual = decode_val(Value.null_val, null(ValueWasNull))
    actual == Ok(ValueWasNull)

## Tag union of possible errors when applying a recipe to a `Value`.
##
## `ParseErr` - The Str or bytes could not be parsed as Json.
##
## TODO: list the errors
Error : [
    ParseErr [
            Leftover (List U8),
            TooShort,
        ],
    RecipeErr RecipeError,
]

RecipeError : [
    Failure FailureReason Value,
    InField Str RecipeError,
    InIndex U64 RecipeError,
]

FailureReason : [
    ExpectedString,
    ExpectedBool,
    ExpectedNull,
    ExpectedNumber,
    OutOfBounds,
    ExpectedArray,
    ExpectedObject,
    ExpectedObjectWithField Str,
    ExpectedArrayWithIndex U64,
]

## An opaque data type for attempting to turn a `Value` into your own data type.
Recipe output := Value -> Result output RecipeError

# Primitives

## Decode a Json string into a Roc `Str`.
string : Recipe Str
string = @Recipe |@Value(val)|
    when val is
        String str -> Ok str
        _ -> Err(Failure(ExpectedString, @Value(val)))

expect
    actual = decode_str("\"test\"", string)
    actual == Ok("test")
expect
    actual = decode_str("42", string)
    Result.is_err(actual)

## Decode a Json boolean into a Roc `Bool`.
bool : Recipe Bool
bool = @Recipe |@Value(val)|
    when val is
        Boolean b -> Ok b
        _ -> Err(Failure(ExpectedBool, @Value(val)))

expect
    actual = decode_str("true", bool)
    actual == Ok(Bool.true)
expect
    actual = decode_str("false", bool)
    actual == Ok(Bool.false)
expect
    actual = decode_str("42", bool)
    Result.is_err(actual)

## Decode a Json number into a Roc `I8`. Silently discards decimals. Will fail if the number is out of range.
i8 : Recipe I8
i8 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_i8_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("42", i8)
    actual == Ok(42i8)

expect
    actual = decode_str("-5", i8)
    actual == Ok(-5i8)

expect
    actual = decode_str("5.7", i8)
    actual == Ok(5i8)

expect
    actual = decode_str("200", i8)
    Result.is_err(actual)

## Decode a Json number into a Roc `I16`. Silently discards decimals. Will fail if the number is out of range.
i16 : Recipe I16
i16 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_i16_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("-32768", i16)
    actual == Ok(-32768i16)

expect
    actual = decode_str("32767", i16)
    actual == Ok(32767i16)

expect
    actual = decode_str("32.8", i16)
    actual == Ok(32i16)

expect
    actual = decode_str("32768", i16)
    Result.is_err(actual)

## Decode a Json number into a Roc `I32`. Silently discards decimals. Will fail if the number is out of range.
i32 : Recipe I32
i32 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_i32_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("-42", i32)
    actual == Ok(-42i32)

expect
    actual = decode_str("42", i32)
    actual == Ok(42i32)

expect
    actual = decode_str("42.8", i32)
    actual == Ok(42i32)

expect
    actual = decode_str("4294967296", i32)
    Result.is_err(actual)

## Decode a Json number into a Roc `I64`. Silently discards decimals. Will fail if the number is out of range.
## Under the hood, parses into a `Dec` and then uses Num.floor and Num.to_i64_checked, so there is some
## weirdness with some numbers (like 92233720368547759000).
i64 : Recipe I64
i64 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_i64_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("42", i64)
    actual == Ok(42i64)
expect
    actual = decode_str("-42", i64)
    actual == Ok(-42i64)
expect
    actual = decode_str("42.6", i64)
    actual == Ok(42i64)
expect
    actual = decode_str("170141183460469231731.687303715884105727", i64)
    Result.is_err(actual)

## Decodes a Json number into a Roc `U8`. Silently discards decimals. Will fail if the number is out of range.
u8 : Recipe U8
u8 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_u8_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("255", u8)
    actual == Ok(255u8)

expect
    actual = decode_str("-1", u8)
    Result.is_err(actual)

expect
    actual = decode_str("256", u8)
    Result.is_err(actual)

## Decodes a Json number into a Roc `U16`. Silently discards decimals. Will fail if the number is out of range.
u16 : Recipe U16
u16 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_u16_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("65535", u16)
    actual == Ok(65535u16)

expect
    actual = decode_str("-1", u16)
    Result.is_err(actual)

expect
    actual = decode_str("65536", u16)
    Result.is_err(actual)

## Decodes a Json number into a Roc `U32`. Silently discards decimals. Will fail if the number is out of range.
u32 : Recipe U32
u32 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_u32_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("4294967295", u32)
    actual == Ok(4294967295u32)

expect
    actual = decode_str("4294967296", u32)
    Result.is_err(actual)

expect
    actual = decode_str("-1", u32)
    Result.is_err(actual)

## Decodes a Json number into a Roc `U64`. Silently discards decimals. Will fail if the number is out of range.
u64 : Recipe U64
u64 = @Recipe |@Value(val)|
    when val is
        Number n -> n |> Num.floor |> Num.to_u64_checked |> Result.map_err(|e| Failure(e, @Value(val)))
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("8446744073709550000", u64)
    actual == Ok(8446744073709550000)
expect
    actual = decode_str("-1", u64)
    Result.is_err(actual)

## Decodes a Json number into a Roc `Dec`.
dec : Recipe Dec
dec = @Recipe |@Value(val)|
    when val is
        Number x -> Ok(x)
        _ -> Err(Failure(ExpectedNumber, @Value(val)))

expect
    actual = decode_str("42", dec)
    actual == Ok(42)
expect
    actual = decode_str("42.5", dec)
    actual == Ok(42.5)
expect
    actual = decode_str("123456789.0123456789", dec)
    actual == Ok(123456789.0123456789)
expect
    actual = decode_str("-1", dec)
    actual == Ok(-1)
expect
    actual = decode_str("-1.5", dec)
    actual == Ok(-1.5)

# Data Structures

## Decodes a Json array into a Roc `List`.
list : Recipe a -> Recipe (List a)
list = |@Recipe inner_recipe|
    @Recipe |@Value(val)|
        when val is
            Array items -> List.map_try(items, inner_recipe)
            _ -> Err(Failure(ExpectedArray, @Value(val)))

expect
    actual = decode_str("[]", list(dec))
    actual == Ok([])

expect
    actual = decode_str("[1, 2, 3]", list(dec))
    actual == Ok([1, 2, 3])

expect
    actual = decode_str("{}", list(dec))
    Result.is_err(actual)

## Decodes a Json object into a Roc `Dict`.
dict : Recipe a -> Recipe (Dict Str a)
dict = |@Recipe inner_recipe|
    @Recipe |@Value(val)|
        when val is
            Object obj ->
                Dict.walk_until obj (Ok(Dict.empty({}))) |so_far, key, inner_val|
                    when so_far is
                        Ok so_far_dict ->
                            when inner_recipe(inner_val) is
                                Ok(item) -> Continue(Ok(Dict.insert(so_far_dict, key, item)))
                                Err(e) -> Break(Err(e))

                        Err(e) -> Break(Err(e))

            _ -> Err(Failure(ExpectedObject, @Value(val)))

expect
    actual = decode_str("{}", dict(dec))
    actual == Ok(Dict.empty({}))

expect
    json_str =
        """
        {"a":42,"b":0.5}
        """
    actual = decode_str(json_str, dict(dec))
    expected = Dict.from_list([("a", 42), ("b", 0.5)])
    actual == Ok(expected)

## Decodes a Json object into a Roc `List` of pairs.
key_value_pairs : Recipe a -> Recipe (List (Str, a))
key_value_pairs = |@Recipe inner_recipe|
    @Recipe |@Value(val)|
        when val is
            Object obj ->
                Dict.walk_until obj (Ok(List.with_capacity(Dict.len(obj)))) |so_far, key, inner_val|
                    when so_far is
                        Ok so_far_list ->
                            when inner_recipe(inner_val) is
                                Ok(item) -> Continue(Ok(List.append(so_far_list, (key, item))))
                                Err(e) -> Break(Err(e))

                        Err(e) -> Break(Err(e))

            _ -> Err(Failure(ExpectedObject, @Value(val)))

expect
    actual = decode_str("{}", key_value_pairs(dec))
    actual == Ok([])

expect
    json_str =
        """
        {"a":42,"b":0.5}
        """
    actual = decode_str(json_str, key_value_pairs(dec))
    expected = [("a", 42), ("b", 0.5)]
    actual == Ok(expected)

# Object Primitives

## For use with [Record Builder](https://www.roc-lang.org/examples/RecordBuilder/README.html).
## This is identical to `map2`, but the Record Builder looks nicer with this name.
build_record : Recipe a, Recipe b, (a, b -> c) -> Recipe c
build_record = |@Recipe inner_a, @Recipe inner_b, fun|
    @Recipe |val|
        a = inner_a(val)?
        b = inner_b(val)?
        Ok(fun(a, b))

expect
    json_str =
        """
        {"foo":"bar","baz":0.5}
        """
    recipe = { build_record <-
        foo: field(string, "foo"),
        baz: field(dec, "baz"),
    }
    actual = decode_str(json_str, recipe)
    expected = { foo: "bar", baz: 0.5 }
    actual == Ok(expected)

## Decode a required field from a Json object.
field : Recipe a, Str -> Recipe a
field = |@Recipe inner_recipe, field_name|
    @Recipe |@Value(val)|
        when val is
            Object obj ->
                field_val = Dict.get(obj, field_name) ? |_| Failure(ExpectedObjectWithField(field_name), @Value(val))
                item = inner_recipe(field_val) ? |e| InField(field_name, e)
                Ok(item)

            _ -> Err(Failure(ExpectedObjectWithField(field_name), @Value(val)))

expect
    json_str =
        """
        {"foo":"bar","baz":0.5}
        """
    recipe = field(string, "foo")
    actual = decode_str(json_str, recipe)
    actual == Ok("bar")

expect
    json_str =
        """
        {"foo":"bar","baz":0.5}
        """
    recipe = field(dec, "baz")
    actual = decode_str(json_str, recipe)
    actual == Ok(0.5)

## Decode a field from a nested Json object, requiring all fields to be present.
at : Recipe a, List Str -> Recipe a
at = |@Recipe inner_recipe, path|
    @Recipe |@Value(val)|
        at_help = |val_so_far, path_so_far|
            when path_so_far is
                [] ->
                    # We have arrived at the target, time to use the inner recipe
                    inner_recipe(@Value(val_so_far))

                [first, .. as rest] ->
                    # We need to keep going
                    when val_so_far is
                        Object obj ->
                            @Value(inner_val) = Dict.get(obj, first) ? |_| Failure(ExpectedObjectWithField(first), @Value(val_so_far))
                            item = at_help(inner_val, rest) ? |e| InField(first, e)
                            Ok(item)

                        _ -> Err(Failure(ExpectedObjectWithField(first), @Value(val_so_far)))
        at_help(val, path)

expect
    json_str =
        """
        {
            "foo": {
                "bar": {
                    "baz": 42
                }
            }
        }
        """
    recipe = at(dec, ["foo", "bar", "baz"])
    actual = decode_str(json_str, recipe)
    actual == Ok(42)

## Decode something from a specific index in a Json array.
index : Recipe a, U64 -> Recipe a
index = |@Recipe inner_recipe, i|
    @Recipe |@Value(val)|
        when val is
            Array arr ->
                inner_val = List.get(arr, i) ? |_| Failure(ExpectedArrayWithIndex(i), @Value(val))
                item = inner_recipe(inner_val) ? |e| InIndex(i, e)
                Ok(item)

            _ -> Err(Failure(ExpectedArrayWithIndex(i), @Value(val)))

expect
    actual = decode_str("[1,true,null]", index(bool, 1))
    actual == Ok(Bool.true)

expect
    actual = decode_str("[1,true,null]", index(dec, 0))
    actual == Ok(1)

expect
    actual = decode_str("[1,true,null]", index(dec, 3))
    Result.is_err(actual)

# More advanced object

## Decode an optional field from a Json object. You must provide the value to use if the field is missing.
optional_field : Recipe a, Str, a -> Recipe a
optional_field = |@Recipe inner_recipe, field_name, default_item|
    @Recipe |@Value(val)|
        when val is
            Object obj ->
                when Dict.get(obj, field_name) is
                    Ok(inner_val) -> inner_recipe(inner_val)
                    Err(KeyNotFound) -> Ok(default_item)

            _ -> Err(Failure(ExpectedObject, @Value(val)))

expect
    json_str = "{}"
    recipe = optional_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(0)

expect
    json_str =
        """
        {"foo": 42}
        """
    recipe = optional_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(42)

expect
    json_str =
        """
        {"foo": null}
        """
    recipe = optional_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    Result.is_err(actual)

expect
    json_str = "null"
    recipe = optional_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    Result.is_err(actual)

## Decode a field that may be `null` from a Json object. You must provide the value to use if the field is `null`.
nullable_field : Recipe a, Str, a -> Recipe a
nullable_field = |@Recipe inner_recipe, field_name, default_item|
    @Recipe |@Value(val)|
        when val is
            Object(obj) ->
                inner_val = Dict.get(obj, field_name) ? |_| Failure(ExpectedObjectWithField(field_name), @Value(val))
                when inner_val is
                    @Value(Null) -> Ok(default_item)
                    _ ->
                        item = inner_recipe(inner_val) ? |e| InField(field_name, e)
                        Ok(item)

            _ -> Err(Failure(ExpectedObjectWithField(field_name), @Value(val)))

expect
    json_str = "{}"
    recipe = nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    Result.is_err(actual)

expect
    json_str =
        """
        {"foo": 42}
        """
    recipe = nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(42)

expect
    json_str =
        """
        {"foo": null}
        """
    recipe = nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(0)

expect
    json_str = "null"
    recipe = nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    Result.is_err(actual)

## Decode an optional field that may be `null` from a Json object. You must provide the value to use if the field is missing or is `null`.
optional_nullable_field : Recipe a, Str, a -> Recipe a
optional_nullable_field = |@Recipe inner_recipe, field_name, default_item|
    @Recipe |@Value(val)|
        when val is
            Object obj ->
                when Dict.get(obj, field_name) is
                    Ok(@Value(Null)) -> Ok(default_item)
                    Err(KeyNotFound) -> Ok(default_item)
                    Ok(inner_val) ->
                        item = inner_recipe(inner_val) ? |e| InField(field_name, e)
                        Ok(item)

            _ -> Err(Failure(ExpectedObject, @Value(val)))

expect
    json_str = "{}"
    recipe = optional_nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(0)

expect
    json_str =
        """
        {"foo": 42}
        """
    recipe = optional_nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(42)

expect
    json_str =
        """
        {"foo": null}
        """
    recipe = optional_nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    actual == Ok(0)

expect
    json_str = "null"
    recipe = optional_nullable_field(dec, "foo", 0)
    actual = decode_str(json_str, recipe)
    Result.is_err(actual)

# Inconsistent Structure

## Try multiple decoders until one succeeds.
one_of : List (Recipe a) -> Recipe a

# Mapping
map : Recipe a, (a -> b) -> Recipe b
map = |@Recipe inner_recipe_a, f|
    @Recipe |val| inner_recipe_a val |> Result.map_ok f

map2 : Recipe a, Recipe b, (a, b -> c) -> Recipe c

map3 : Recipe a, Recipe b, Recipe c, (a, b, c -> d) -> Recipe d

map4 : Recipe a, Recipe b, Recipe c, Recipe d, (a, b, c, d -> e) -> Recipe e

map5 : Recipe a, Recipe b, Recipe c, Recipe d, Recipe e, (a, b, c, d, e -> f) -> Recipe f

map6 : Recipe a, Recipe b, Recipe c, Recipe d, Recipe e, Recipe f, (a, b, c, d, e, f -> g) -> Recipe g

map7 : Recipe a, Recipe b, Recipe c, Recipe d, Recipe e, Recipe f, Recipe g, (a, b, c, d, e, f, g -> h) -> Recipe h

map8 : Recipe a, Recipe b, Recipe c, Recipe d, Recipe e, Recipe f, Recipe g, Recipe h, (a, b, c, d, e, f, g, h -> i) -> Recipe i

# Fancy decoding

## "Decode" this Json `Value` as a `Value`. Perhaps you are going to finish decoding it later.
value : Recipe Value

## Decode a `null` Json value into some Roc value of your choice.
null : a -> Recipe a
null = |a|
    @Recipe |@Value(val)|
        when val is
            Null -> Ok(a)
            _ -> Err(Failure ExpectedNull @Value(val))

## Successfully "decode" the provided value, regardless of the Json.
succeed : a -> Recipe a

## Always fail. You provide the error message.
fail : Str -> Recipe a

## Create decoders where the next stage depends on the results of the earlier stage.
and_then : Recipe a, (a -> Recipe b) -> Recipe b

# Encoding

## Create a `Value` from a `Str`. This can be useful if you want to encode variably-shaped data.
from_string : Str -> Value
from_string = |text| @Value(String(text))

from_bool : Bool -> Value
from_bool = |b| @Value(Boolean(b))

from_i8 : I8 -> Value

from_i16 : I16 -> Value

from_i32 : I32 -> Value

from_i64 : I64 -> Value

from_u8 : U8 -> Value

from_u16 : U16 -> Value

from_u32 : U32 -> Value

from_u64 : U64 -> Value

from_dec : Dec -> Value
from_dec = |x| @Value(Number x)

null_val : Value
null_val = @Value(Null)

from_list : List a, (a -> Value) -> Value

from_list_val : List Value -> Value

from_object : List (Str, Value) -> Value

from_dict : Dict key value, (key -> Str, (value -> Value)) -> Value
