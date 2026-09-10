use Test;

# This test requires JSON::Tiny. Run with:
#   prove -e 'target/debug/mutsu -I tmp/json-tiny/lib' t/json-tiny-compat.t
# Or: MUTSULIB=tmp/json-tiny/lib prove -e 'target/debug/mutsu' t/json-tiny-compat.t
#
# Automatically skipped when JSON::Tiny module files are not present.

unless "tmp/json-tiny/lib/JSON/Tiny.pm".IO.e {
    plan 1;
    skip "JSON::Tiny not installed (run: git clone https://github.com/moritz/json.git tmp/json-tiny)", 1;
    done-testing;
    exit 0;
}

use lib 'tmp/json-tiny/lib';
use JSON::Tiny;

plan 48;

# to-json: numbers
is to-json(42), '42', 'to-json integer';
is to-json(3.14), '3.14', 'to-json decimal';

# to-json: strings
is to-json("hello world"), '"hello world"', 'to-json simple string';
is to-json("with \"quotes\""), '"with \\"quotes\\""', 'to-json string with quotes';
is to-json("line\nbreak"), '"line\\nbreak"', 'to-json string with newline';

# to-json: booleans and null
is to-json(True), 'true', 'to-json True';
is to-json(False), 'false', 'to-json False';
is to-json(Any), 'null', 'to-json Any (null)';

# to-json: arrays
is to-json([1, 2, 3]), '[ 1, 2, 3 ]', 'to-json array of ints';
is to-json(["a", "b"]), '[ "a", "b" ]', 'to-json array of strings';

# to-json: hashes
{
    my $json = to-json({name => "test"});
    ok $json.contains('"name"'), 'to-json hash contains key';
    ok $json.contains('"test"'), 'to-json hash contains value';
}

# to-json: nested structures
{
    my $json = to-json({list => [1, 2], flag => True});
    ok $json.contains('[ 1, 2 ]'), 'to-json nested array in hash';
    ok $json.contains('true'), 'to-json nested bool in hash';
}

# from-json: numbers
is from-json('42'), 42, 'from-json integer';
is from-json('3.14'), 3.14, 'from-json decimal';
is from-json('-7'), -7, 'from-json negative integer';

# from-json: booleans and null
is from-json('true'), True, 'from-json true';
is from-json('false'), False, 'from-json false';
ok !from-json('null').defined, 'from-json null is undefined';

# from-json: arrays
is-deeply from-json('[1, 2, 3]'), [1, 2, 3], 'from-json array of ints';
is-deeply from-json('[true, false, null]'), [True, False, Any], 'from-json array of mixed';
is-deeply from-json('[[1, 2], [3]]'), [[1, 2], [3]], 'from-json nested arrays';

# from-json: grammar parses JSON structures (smoke test)
ok JSON::Tiny::Grammar.parse('{"a": 1}').defined, 'grammar parses JSON object';
ok JSON::Tiny::Grammar.parse('[1, 2]').defined, 'grammar parses JSON array';
ok JSON::Tiny::Grammar.parse('"hello"').defined, 'grammar parses JSON string';

# from-json: strings
is from-json('"hello"'), 'hello', 'from-json simple string';
is from-json('"hello world"'), 'hello world', 'from-json string with space';

# from-json: array of strings
is-deeply from-json('["a", "b", "c"]'), ["a", "b", "c"], 'from-json array of strings';

# Grammar str_escape token (used internally by string parsing)
ok JSON::Tiny::Grammar.subparse('n', :rule<str_escape>).defined,
   'grammar str_escape token matches n';

# from-json: objects (hashes)
{
    my $d = from-json('{"name":"test","age":42}');
    is $d.WHAT.gist, '(Hash)', 'from-json object returns Hash';
    is $d<name>, 'test', 'from-json object string value';
    is $d<age>, 42, 'from-json object integer value';
}

# from-json: nested objects
{
    my $d = from-json('{"a":[1,2],"b":{"c":3}}');
    is-deeply $d<a>, [1, 2], 'from-json nested array in object';
    is $d<b><c>, 3, 'from-json nested object access';
}

# from-json: object with boolean/null values
{
    my $d = from-json('{"flag":true,"empty":null,"off":false}');
    is $d<flag>, True, 'from-json object bool true';
    is $d<off>, False, 'from-json object bool false';
    ok !$d<empty>.defined, 'from-json object null value';
}

# from-json: empty object
{
    my $d = from-json('{}');
    is $d.WHAT.gist, '(Hash)', 'from-json empty object is Hash';
    is $d.elems, 0, 'from-json empty object has no elements';
}

# from-json: object with array values
{
    my $d = from-json('{"items":[10,20,30]}');
    is-deeply $d<items>, [10, 20, 30], 'from-json object with array value';
}

# from-json: escaped characters in strings (requires class-level my %h fix)
is-deeply from-json('["\""]'), ['"'], 'from-json escaped quote';
is-deeply from-json('["\\\\" ]'), ['\\'], 'from-json escaped backslash';
is-deeply from-json('["\\/"]'), ['/'], 'from-json escaped slash';
is-deeply from-json('["\\t\\n"]'), ["\t\n"], 'from-json escaped tab and newline';

# from-json: unicode escapes (requires utf16.new Seq fix)
is-deeply from-json('["\\u2685"]'), ["\x[2685]"], 'from-json \\uXXXX unicode escape';
{
    my $d = from-json('{ "a" : "b\\u00E5" }');
    is $d<a>, "b\x[E5]", 'from-json unicode escape in object value';
}

# Grammar parse: deeply nested structures (requires larger stack)
ok JSON::Tiny::Grammar.parse('[[[[[[[[[[[[[[[[[[["deep"]]]]]]]]]]]]]]]]]]]').defined,
   'grammar parses deeply nested arrays without stack overflow';
