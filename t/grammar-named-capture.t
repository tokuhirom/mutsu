use Test;

plan 14;

# Repeated named subrule with quantifier and separator
{
    grammar G {
        token TOP { <word>+ % \s+ }
        token word { \w+ }
    }
    my $m = G.parse("hello world");
    ok $m.defined, 'grammar with <word>+ % \\s+ parses';
    is $m<word>.elems, 2, '<word> captures 2 elements';
    is ~$m<word>[0], 'hello', '<word>[0] is hello';
    is ~$m<word>[1], 'world', '<word>[1] is world';
}

# Three words
{
    grammar G3 {
        token TOP { <word>+ % \s+ }
        token word { \w+ }
    }
    my $m = G3.parse("a b c");
    ok $m.defined, 'grammar with 3 words parses';
    is $m<word>.elems, 3, '<word> captures 3 elements';
    is ~$m<word>[0], 'a', '<word>[0] is a';
    is ~$m<word>[2], 'c', '<word>[2] is c';
}

# Multiple same-name subrules (non-quantified)
{
    grammar G2 {
        token TOP { <num> '-' <num> }
        token num { \d+ }
    }
    my $m2 = G2.parse("42-99");
    ok $m2.defined, 'grammar with two <num> parses';
    is $m2<num>.elems, 2, '<num> captures 2 elements';
    is ~$m2<num>[0], '42', '<num>[0] is 42';
    is ~$m2<num>[1], '99', '<num>[1] is 99';
}

# Single named subrule (non-repeated)
{
    grammar G4 {
        token TOP { <word> }
        token word { \w+ }
    }
    my $m = G4.parse("hello");
    ok $m.defined, 'single <word> parses';
    is ~$m<word>, 'hello', '<word> is hello for single match';
}
