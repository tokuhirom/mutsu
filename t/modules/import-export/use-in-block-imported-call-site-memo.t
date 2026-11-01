use Test;
use lib 't/lib';

# A call site of a lexically imported routine remembers the routine its name
# resolved to (#14371). The memo must follow a rebinding: two blocks importing
# different routines of the same name, a `use` that runs again on every
# iteration, and an inner `use` that shadows an outer one. Checked against
# rakudo.
plan 5;

my @r;
for 1..3 -> $i {
    { use ImportSiteMemoA; @r.push: who($i); }
    { use ImportSiteMemoB; @r.push: who($i); }
}
is @r.join(','), 'A1,B1,A2,B2,A3,B3', 'two blocks bind the same name to different routines';

{
    use ImportSiteMemoA;
    my @hot = (^5).map: { who($_) };
    is @hot.join(','), 'A0,A1,A2,A3,A4', 'a hot call site keeps answering the imported routine';
    {
        use ImportSiteMemoB;
        is who(1), 'B1', 'an inner use shadows the outer one';
    }
    is who(2), 'A2', 'the outer binding is back after the inner block';
}

my @again;
for 1..2 { use ImportSiteMemoA; @again.push: who($_); }
is @again.join(','), 'A1,A2', 'a use inside a loop body rebinds on every iteration';
