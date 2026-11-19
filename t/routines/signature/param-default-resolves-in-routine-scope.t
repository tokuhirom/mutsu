use Test;

# A routine declared at the top level resolves names in its own lexical scope
# -- its body and its parameter defaults alike -- never in a mainline block
# that happens to call it (#17630, ADR-12529 phase 3).

plan 8;

my $hid = 'outer-hid';
sub default-op(:&cmp = &[cmp]) { cmp(2, 1) }
sub default-positional(&c = &[cmp]) { c(2, 1) }
sub default-var($x = $hid) { $x }
sub body-op { &infix:<cmp>(2, 1) }
sub body-op-value { my &c = &[cmp]; c(2, 1) }
sub body-infix { 2 cmp 1 }

is default-op(), More, 'a named default outside any shadowing block';
{
    my &infix:<cmp> = -> $a, $b { 'inner' };
    is default-op(), More, 'a named default does not see the calling block\'s operator';
    is default-positional(), More, 'nor does a positional default';
    is body-op(), More, 'the body\'s &infix:<cmp> is the core operator';
    is body-op-value(), More, 'a &[cmp] value taken in the body too';
    is body-infix(), More, 'and so is an infix call in the body';
}
{
    my $hid = 'block-hid';
    is default-var(), 'outer-hid', 'a default reads the routine\'s own outer variable';
}

# The calling block's operator is still its own inside the block.
{
    my &infix:<cmp> = -> $a, $b { 'inner' };
    is (2 cmp 1), 'inner', 'the block itself sees its operator';
}
