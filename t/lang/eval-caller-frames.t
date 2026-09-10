use Test;

plan 10;

# EVAL runs the compiled unit behind two intermediate caller frames (rakudo:
# the EVAL multi candidate and its proto), so from EVAL'd mainline code the
# scope that invoked EVAL is reached at CALLER::CALLER::CALLER::, and the
# first two hops see frames with no user lexicals. A CALLER:: lookup that
# misses (unknown name, or walking past the top of the stack) yields Nil
# quietly; only a symbol that is present but not dynamic throws
# X::Caller::NotDynamic. All pinned against rakudo (tmp/otf-eval probes,
# 2026-07-12).

# depth 1 = EVAL's own frame: the invoking sub's lexicals are invisible.
sub d1() { my $z = 11; EVAL '(CALLER::<$z>).raku' }
is d1(), 'Nil', 'EVAL CALLER:: does not see the invoking scope at depth 1';

sub d2() { my $z = 11; EVAL '(CALLER::CALLER::<$z>).raku' }
is d2(), 'Nil', 'EVAL CALLER::CALLER:: is still inside the EVAL frames';

# depth 3 reaches the sub that invoked EVAL.
sub d3-dyn() { my $*dz = 17; EVAL 'CALLER::CALLER::CALLER::<$*dz>' }
is d3-dyn(), 17, 'EVAL depth 3 reaches the invoking sub (dynamic var)';

throws-like { my $z = 11; $z++; EVAL 'CALLER::CALLER::CALLER::<$z>' },
    X::Caller::NotDynamic, symbol => '$z',
    'EVAL depth 3 non-dynamic lexical of the invoking scope throws';

sub d4() { my $*dz = 17; EVAL '(CALLER::CALLER::CALLER::CALLER::<$*dz>).raku' }
is d4(), 'Nil', 'EVAL depth 4 walks past the invoking sub';

# EVAL invoked from mainline: same frame layout.
my $*md = 42;
is EVAL('CALLER::CALLER::CALLER::<$*md>'), 42,
    'EVAL from mainline reaches mainline dynamics at depth 3';
is EVAL('(CALLER::CALLER::CALLER::CALLER::<$*md>).raku'), 'Nil',
    'EVAL from mainline: depth 4 walks past the top quietly';

# Non-EVAL CALLER:: misses are quiet Nil, not errors.
sub nf-angle() { (CALLER::<$nosuchvar>).raku }
is nf-angle(), 'Nil', 'CALLER::<$x> miss yields Nil quietly';

sub nf-sigil() { ($CALLER::nosuchvar2).raku }
is nf-sigil(), 'Nil', '$CALLER::x miss yields Nil quietly';

sub nf-deep() { (CALLER::CALLER::CALLER::CALLER::CALLER::<$xx>).raku }
is nf-deep(), 'Nil', 'CALLER:: past the top of the stack yields Nil quietly';
