use Test;

# ADR-0032: WrapVarRef container capture is a property of the capture edge,
# not of the named-sub declaration form. This is a SIBLING of
# t/captured-outer-pair-container-alias.t (which pins the named-sub shapes
# the old mechanism already handled and must not be rewritten) — this file
# covers every OTHER reader kind the old GetGlobal+WrapVarRef peephole scan
# never saw: a pointy block, an anon `sub {}`, a bare block, a class method,
# an escaping closure, and non-Pair consumers (`\($v)` Capture). Probe
# letters match docs/adr/0032-wrapvarref-container-capture-across-closure-boundaries.md
# §1.4/§7.

plan 19;

# Probe C: pointy block, read liveness.
{
    my $v = 1;
    my $mk = -> { key => $v };
    my $p = $mk();
    $v = 2;
    is $p.value, 2, 'C: pointy block key=>$v retains the outer container (read)';
}

# Probe B: pointy block, write-through.
{
    my $v = 1;
    my $mk = -> { key => $v };
    my $p = $mk();
    $p.value = 9;
    is $v, 9, 'B: pointy block key=>$v writes through to the outer scalar';
}

# Probe H: pointy block, Pair.new.
{
    my $v = 1;
    my $mk = -> { Pair.new("k", $v) };
    my $p = $mk();
    $v = 2;
    is $p.value, 2, 'H: pointy block Pair.new retains the outer container';
}

# Probe F: class method.
{
    my $v = 1;
    class ADR0032F { method mk() { key => $v } }
    my $p = ADR0032F.mk;
    $p.value = 9;
    is $v, 9, 'F: class method key=>$v writes through to the outer scalar';
}

# Probe K: the closure itself escapes (stored in an array).
{
    my $v = 1;
    my @cb = (-> { key => $v },);
    my $p = @cb[0]();
    $v = 2;
    is $p.value, 2, 'K: an escaping closure Pair retains the outer container';
}

# Probe Z2 (J): a cell provably exists (captured-and-mutated by ADR-0025's
# own escape analysis) and Half B must still not strip it.
{
    my $v = 1;
    my @cb = (-> { my $p = (key => $v); $v = $v; $p },);
    my $p = @cb[0]();
    $v = 2;
    is $p.value, 2, 'Z2: an already-boxed captured scalar still retains its container';
}

# Probe L / Z4: named-sub reader whose owning frame is TWO frames up (a
# pointy block wraps the named sub) -- Half A must bubble past the
# intermediate frame that does not own the name.
{
    my $v = 1;
    my $step = -> { sub inner() { key => $v }; inner() };
    my $p = $step();
    $v = 2;
    is $p.value, 2, 'L/Z4: named-sub two frames up still retains the outer container';
}

# Probe T: Capture (`\($v)`) built inside a closure -- proves this is not a
# Pair-specific fix (U is the same-frame control and already passed).
{
    my $v = 1;
    my $mk = -> { \($v) };
    my $c = $mk();
    $v = 7;
    is $c[0], 7, 'T: Capture built inside a closure aliases the outer scalar';
}
{
    my $v = 1;
    my $c = \($v);
    $v = 7;
    is $c[0], 7, 'U (control): same-frame Capture aliases the outer scalar';
}

# Probe X: `.VAR.WHICH` identity across a closure boundary. NOT fixed by
# this ADR -- `.VAR` never goes through WrapVarRef (it reads the target via
# a plain GetGlobal/GetLocal, already dereferenced, and its reflection
# object's identity comes from a separate name-keyed `var_meta_value` env
# cache that has no cross-frame writeback of its own). Confirmed a
# pre-existing, unrelated bug: even the named-sub mechanism this ADR
# generalizes (which predates it) fails the same shape. See
# todo/tickets/var-which-identity-across-closure-boundary.md.
{
    my $v = 1;
    my $mk = -> { $v.VAR.WHICH };
    todo 'X: .VAR.WHICH cross-closure identity is a separate, pre-existing bug (see todo/tickets/var-which-identity-across-closure-boundary.md)';
    is $mk(), $v.VAR.WHICH, 'X: .VAR.WHICH identity matches across a closure boundary';
}

# Probe Y: a closure-built Pair and a same-frame Pair must share ONE
# container.
{
    my $v = 1;
    my $mk = -> { (k => $v) };
    my $p1 = $mk();
    my $p2 = (k => $v);
    $p1.value = 3;
    is $v, 3, 'Y: closure-built Pair writes through to the shared outer scalar';
    is $p2.value, 3, 'Y: a same-frame Pair built afterwards sees the same write';
}

# Probe N: file-scope pointy block (no enclosing bare block).
my $adr0032_n = 1;
my $adr0032_n_mk = -> { key => $adr0032_n };
my $adr0032_n_pair = $adr0032_n_mk();
$adr0032_n = 2;
is $adr0032_n_pair.value, 2, 'N: file-scope pointy block retains the outer container';

# Probe Q: pointy block inside a sub.
sub adr0032-q() {
    my $v = 1;
    my $mk = -> { key => $v };
    my $p = $mk();
    $v = 2;
    $p.value;
}
is adr0032-q(), 2, 'Q: pointy block inside a sub retains the outer container';

# Probe 5b: anonymous `sub {}` (not a pointy block).
{
    my $v = 1;
    my $mk = sub { key => $v };
    my $p = $mk();
    $v = 2;
    is $p.value, 2, '5b: anon sub {} key=>$v retains the outer container';
}

# --- Controls: must keep passing (were already correct before ADR-0032). ---

# Probe A: a plain closure read (no container capture) sees live mutation.
{
    my $v = 1;
    my $mk = -> { $v };
    $v = 2;
    is $mk(), 2, 'A (control): plain closure read observes the mutated value';
}

# Probe V: an `is rw` argument passed from inside a closure still writes
# through (the write-path machinery, unaffected by this ADR).
sub adr0032-bump($x is rw) { $x = $x + 1 }
{
    my $v = 1;
    my $mk = -> { adr0032-bump($v) };
    $mk();
    is $v, 2, 'V (control): is rw argument from inside a closure writes through';
}

# Probe W: a `:=` bind performed inside a closure still aliases.
{
    my $v = 1;
    my $b;
    my $mk = -> { $b := $v };
    $mk();
    $v = 5;
    is $b, 5, 'W (control): := bind performed inside a closure aliases the outer scalar';
}

# Shadow-safety negative control (D1's widening condition): a same-named
# INNER shadow must never be picked up by an outer WrapVarRef site --
# mirrors t/list-alias-shadowed-name.t test 8, restated here as the direct
# regression for D1's "not a local of the EMITTING frame" rule.
{
    my $v = 10;
    { my $v = 99; }
    my $pair = (k => $v);
    is $pair.value, 10, 'shadow safety: an inner shadowed $v does not leak into an outer WrapVarRef';
}
