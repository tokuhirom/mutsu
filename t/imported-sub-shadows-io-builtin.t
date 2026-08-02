use v6;
use lib 't/lib';
use Test;
use IOBuiltinShadow;

plan 5;

# A `sub` named like an IO builtin (`say`/`print`/`put`/`note`) shadows the
# builtin's listop STATEMENT form in its lexical scope. A locally-declared sub
# already did; an *imported* one did not, so `put -> 'x' { }` after
# `use Cro::HTTP::Router` (which exports the HTTP verb `put`) parsed as a print
# of the block and the route was never registered.

put -> 'product' { 1 }
is shadow-log(), 'put', 'an imported `put` wins over the IO builtin';

note-it("hello");
is shadow-log(), 'put,note-it', 'a plain imported sub is unaffected';

# Still callable in every other position.
put(-> 'other' { 2 });
is shadow-log(), 'put,note-it,put', 'the parenthesized call form still works';

# A locally-declared sub keeps shadowing (the pre-existing behaviour).
{
    my @seen;
    sub print(*@a) { @seen.push(@a.join('')) }
    print "local";
    is @seen.join(','), 'local', 'a locally declared `print` still shadows';
}

# An unshadowed builtin is untouched.
{
    my $out = $*OUT;
    is $out.WHAT.^name.chars > 0, True, 'the real IO builtins are still there';
}
