use Test;

plan 2;

# The later package declaration contributes another local named x to the
# compiled chunk. Binding through OUR:: must use the source's resolved slot,
# not the last slot with that spelling (ADR-0097 section 15).
{
    our $x30 = 31;
    my $x = 39;
    $OUR::x30 := $x;
    ok $OUR::x30 =:= $x, 'OUR bind keeps the source container';
    our package A44 { our $x = 41 }
    is $A44::x, 41, 'later package x stays separate';
}
