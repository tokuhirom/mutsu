use v6;
use Test;

plan 8;

# #12637: a sigilless term `r` and a scalar `$r` are separate namespaces,
# also inside a routine whose `\r` parameter re-declares a name an outer
# scope already had shadowed.
{
    my \r = 3;
    my $r = 5;
    is r, 3, 'sigilless r survives a same-named scalar';
    is $r, 5, 'scalar $r keeps its own value';

    sub bind-form(Mu \r) { my $r := 7; (r.raku, $r) }
    is-deeply bind-form(Int), ("Int", 7), 'param \r vs my $r := in a nested sub';

    sub assign-form(Mu \r) { my $r = 9; (r.raku, $r) }
    is-deeply assign-form(Int), ("Int", 9), 'param \r vs my $r = in a nested sub';

    sub read-first(Mu \r) { my $before = r.raku; my $r = 1; ($before, r.raku, $r) }
    is-deeply read-first(Str), ("Str", "Str", 1), 'param \r read before and after my $r';

    is r, 3, 'outer sigilless unchanged after the calls';
}

sub plain(Mu \r) { my $r := 5; (r.raku, $r) }
is-deeply plain(Int), ("Int", 5), 'no outer binding: param \r vs my $r :=';

my \q = 1; my $q = 2;
my $c = -> \q { my $q = 4; (q, $q) };
is-deeply $c(8), (8, 4), 'pointy param \q vs my $q inside a closure';
