unit module BeginPrologueFixture;

# ADR-0134: a module's top-level BEGIN runs in the module's prologue, over
# lexicals in their static state.
my $c = True;
our $seen;
BEGIN $seen = $c.raku;

our @order;
@order.push('run');
BEGIN @order.push('begin');
