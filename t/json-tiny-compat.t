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

plan 30;

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
