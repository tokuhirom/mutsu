use Test;

plan 5;

sub l () { 1, 2 }

# A single hash-key assignment names one scalar slot, so its rvalue is itemized
# (a following list context sees one element), like a scalar-variable assignment.

{
    my %a;
    my @z = (%a<x> = l, l);
    is @z.elems, 1, '%h<x> = l, l used as an rvalue is one itemized element';
    ok !@z[1].defined, 'no second element leaked into the list';
    is %a<x>.elems, 2, 'the whole list (1,2) is stored at the key';
}

{
    my %a;
    my @z = (%a{'x'} = l, l);
    is @z.elems, 1, q{%h{'x'} = l, l is one itemized element};
}

# A scalar value passes through unchanged.
{
    my %a;
    %a<x> = 5;
    is %a<x>, 5, 'a scalar hash-value assignment stores correctly';
}
