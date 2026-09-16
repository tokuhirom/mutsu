use v6;
use Test;

# A custom attribute trait's parenthesized call may take more than one
# positional argument: `is xml-namespace('urn', 'prefix')`. Raku itemizes
# those arguments into a single List when the trait sub's parameter is a
# scalar named parameter (`:$xml-namespace!` sees `$("urn", "prefix")`).
#
# The parser used to hand the whole `'urn', 'prefix'` span to a
# single-expression parser; that parse always failed (leftover `, 'prefix'`),
# so the argument silently fell back to Bool::True — the "unknown trait with
# no argument" case — discarding both values (XML::Class's
# `is xml-namespace('urn', 'prefix')`, #8528).

plan 4;

my $captured;

multi sub trait_mod:<is> (Attribute $attr, :$xml-namespace!) {
    $captured = $xml-namespace;
}

class C {
    has Str $.bar is xml-namespace('urn:foo', 'f');
}

isa-ok $captured, List, 'multi positional trait args are captured as a List';
is $captured.elems, 2, 'both positional trait args survive';
is $captured[0], 'urn:foo', 'first positional trait arg is correct';
is $captured[1], 'f', 'second positional trait arg is correct';

done-testing;
