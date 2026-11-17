use Test;
use lib 't/lib-imported-proto-memo';
use ProtoMemoScope;

# The proto lookups of an imported family are remembered per search context
# and per deciding compunit (#15786). Warm calls must keep answering exactly
# what the first one did: the importer sees the exported families only, the
# module keeps its own, and the module's `my class` stays its own type.
# Every expectation was checked against Rakudo.

plan 12;

my @found;
my @kinds;
my @hidden;
for ^3 {
    @found.push: pms-find(<a b c>, 'b');
    @kinds.push: pms-kind(pms-find(<a b c>, 'z'));
    @hidden.push: pms-call-hidden($_ %% 2 ?? 1 !! 'x');
}
is-deeply @found, [1, 1, 1], 'an imported proto family dispatches on every call';
is-deeply @kinds, ['missing' xx 3],
    "a candidate typed with the module's lexical class matches its instances every time";
is-deeply @hidden, ['h-int', 'h-str', 'h-int'],
    "the module's own code reaches its unexported proto family on every call";

is pms-find({ :a(1) }, 'a'), 'a', 'the hash candidate of the same family';
ok pms-found(pms-find(<a b>, 'a')), 'a found index is not the lexical class';
nok pms-found(pms-find(<a b>, 'q')), 'a miss is an instance of it';

throws-like { EVAL 'pms-hidden(1)' }, X::Undeclared::Symbols,
    'after warm calls the unexported family still does not reach the importer';
nok (try EVAL '&pms-hidden.candidates.elems'), 'nor does its &name';

{
    my class Missing { }
    is pms-kind(Missing.new), 'value',
        "the importer's own class of the same short name is a different type";
    is pms-kind(pms-find(('a',), 'b')), 'missing', 'while the module still sees its own';
}

# A user proto of the same name in an inner scope hides the imported one, and
# the outer scope keeps the imported family.
{
    proto sub pms-find(|) {*}
    multi sub pms-find($x) { "local:$x" }
    is pms-find(5), 'local:5', 'an inner proto of the same name wins in its scope';
}
is pms-find(<x y>, 'y'), 1, 'and the imported family is back outside it';
