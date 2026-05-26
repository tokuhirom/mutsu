use Test;

plan 8;

# Bug 1: Named captures from subrules with * % should be arrays
{
    grammar G1 {
        token ident { \w+ }
        token name { <ident> * % '.' }
    }

    my $m = G1.parse("bar", rule => 'name');
    is $m<ident>.WHAT.raku, 'Array', 'single match: named capture is Array';
    is $m<ident>.elems, 1, 'single match: one element in array';

    my $m2 = G1.parse("foo.bar", rule => 'name');
    is $m2<ident>.WHAT.raku, 'Array', 'multi match: named capture is Array';
    is $m2<ident>.elems, 2, 'multi match: two elements in array';
}

# Bug 2: Separator in * % should not leak into positional captures
{
    grammar G2 {
        token ident { \w+ }
        token name { [<ident> * % '.' | ('.')] }
    }

    my $m = G2.parse("bar", rule => 'name');
    is $m[0].defined, False, 'single match: $0 is undefined (first branch matched)';
    is $m<ident>.WHAT.raku, 'Array', 'single match with alternation: named capture is Array';

    my $m2 = G2.parse("foo.bar", rule => 'name');
    is $m2[0].defined, False, 'multi match: $0 is undefined (separator not captured)';
    is $m2<ident>.elems, 2, 'multi match with alternation: two elements';
}
