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

use JSON::Tiny;

plan 14;

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
