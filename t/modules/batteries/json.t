use Test;
use JSON::Fast;

plan 34;

# --- from-json: scalars ---
is from-json("42"), 42, 'from-json integer';
is from-json("42").^name, 'Int', 'from-json integer is Int';
is from-json("-7"), -7, 'from-json negative integer';
is from-json("2.5"), 2.5, 'from-json decimal';
is from-json("2.5").^name, 'Rat', 'from-json decimal is Rat';
is from-json("1e2").^name, 'Num', 'from-json exponent is Num';
is from-json('"hello"'), 'hello', 'from-json string';
is from-json("true"), True, 'from-json true';
is from-json("false"), False, 'from-json false';
is from-json("null").^name, 'Any', 'from-json null is Any';

# --- from-json: escapes ---
is from-json('"a\nb"'), "a\nb", 'from-json newline escape';
is from-json('"tab\there"'), "tab\there", 'from-json tab escape';
is from-json('"quote\"x"'), 'quote"x', 'from-json quote escape';
is from-json('"A"'), 'A', 'from-json unicode escape';

# --- from-json: structures ---
my $arr = from-json("[1, 2, 3]");
is $arr.^name, 'Array', 'from-json array is Array';
is $arr.elems, 3, 'from-json array elems';
is $arr[1], 2, 'from-json array element';

my $obj = from-json('{"a": 1, "b": "two", "c": [true, null]}');
is $obj.^name, 'Hash', 'from-json object is Hash';
is $obj<a>, 1, 'from-json object int value';
is $obj<b>, 'two', 'from-json object str value';
is $obj<c>[0], True, 'from-json nested array bool';

# --- to-json: scalars ---
is to-json(42), '42', 'to-json integer';
is to-json("str"), '"str"', 'to-json string';
is to-json(True), 'true', 'to-json True';
is to-json(False), 'false', 'to-json False';
is to-json(Any), 'null', 'to-json type object is null';
is to-json(3/4), '0.75', 'to-json Rat';
is to-json(2/1), '2.0', 'to-json integral Rat gets .0';

# --- to-json: escapes ---
is to-json("a\nb"), '"a\nb"', 'to-json escapes newline';
is to-json('say "hi"'), '"say \"hi\""', 'to-json escapes quotes';

# --- to-json: non-pretty / sorted ---
is to-json({b => 1, a => 2}, :!pretty, :sorted-keys), '{"a":2,"b":1}', 'to-json compact sorted';
is to-json([1, 2, 3], :!pretty), '[1,2,3]', 'to-json compact array';

# --- roundtrip ---
my $data = {name => "alice", tags => ["x", "y"], n => 3};
is-deeply from-json(to-json($data)), $data, 'roundtrip preserves data';

# --- empty ---
is to-json({}, :!pretty), '{}', 'to-json empty hash compact';
