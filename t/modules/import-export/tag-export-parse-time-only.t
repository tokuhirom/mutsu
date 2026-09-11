use v6;
use Test;

# The parser's per-module export scan now collects every `is export` sub
# regardless of the tag it carries, because that set answers a parse-time
# question ("is this name a routine?") and the scan is never told which tags
# the importer actually asked for. See `imported-listop-angle-arg.t`.
#
# This file pins the other side of that widening: it is parse-time knowledge
# only. Run-time name resolution still resolves against the real import set,
# so a name the importer's tag list withholds stays uncallable. Without that,
# the widening would silently import names raku does not.
#
# This unit asks for `:extra` alone, so `joined` is imported and `root`
# (exported untagged, i.e. DEFAULT) is not.
#
# https://github.com/tokuhirom/mutsu/issues/7939

plan 3;

use lib 't/lib';
use ImportedListopAngle :extra;

is (joined <a b c>), 'a-b-c',
    'the tag this unit asked for is imported, and parses as a listop';

my $result = try EVAL 'root("abcd")';
nok $result.defined,
    'a DEFAULT-tag name this unit did not ask for is not callable';
is $!.^name, 'X::Undeclared::Symbols',
    'and it fails as an undeclared symbol, not as a call to something else';
