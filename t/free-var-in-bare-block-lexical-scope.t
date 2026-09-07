use Test;

# A named sub's free variables resolve to the binding visible at the sub's
# DECLARATION site, not to whatever a calling frame happens to hold under the
# same name.  ADR-0024 established that for a sub declared at mainline
# (file scope); this file pins the same matrix for a sub declared inside a
# BARE BLOCK, which has no compunit-lexical store of its own.
#
# Every row is measured against rakudo and must agree with it.  The file-scope
# rows (E/K/L) are controls that must not move; the dynamic-variable row (I) is
# the control that must KEEP caller priority, being dynamic-scope by design.

plan 43;

# --- A: block-scope $ read, shadowing caller is a routine ------------------
{
    my $av = 1;
    sub ga() { $av }
    sub fa() { my $av = 5; ga() }
    is fa(), 1, 'A: block-scope $ read ignores the calling routine shadow';
}

# --- B: block-scope $ write ------------------------------------------------
{
    my $bv = 1;
    sub gb() { $bv = 3 }
    sub fb() { my $bv = 5; gb(); $bv }
    is fb(), 5, 'B1: a callee write does not clobber the caller shadow';
    is $bv, 3, 'B2: the write lands on the block lexical the sub captured';
}

# --- C: block-scope $ bind, last-statement (SetGlobal) spelling ------------
{
    my $cv = 1;
    my $ca;
    sub gc1() { $ca := $cv }
    sub fc() { my $cv = 5; gc1(); $cv }
    is fc(), 5, 'C1: a := of a free var does not alias the caller lexical';
    $cv = 9;
    is $ca, 9, 'C2: the alias still tracks the block lexical';
    is $cv, 9, 'C3: the block lexical is unchanged by the bind';
}

# --- D: block-scope $ bind, bind-then-statement (SetLocal) spelling --------
{
    my $dv = 1;
    my $da;
    sub gd() { $da := $dv; 0 }
    sub fd() { my $dv = 5; gd(); $dv }
    is fd(), 5, 'D1: the SetLocal bind spelling behaves the same';
    $dv = 7;
    is $da, 7, 'D2: the alias still tracks the block lexical';
    is $dv, 7, 'D3: the block lexical is unchanged by the bind';
}

# --- E: FILE-SCOPE control of C -------------------------------------------
my $ev = 1;
my $ea;
sub ge() { $ea := $ev }
sub fe() { my $ev = 5; ge(); $ev }
is fe(), 5, 'E1: file-scope control unchanged';
$ev = 9;
is $ea, 9, 'E2: file-scope alias tracks';
is $ev, 9, 'E3: file-scope source unchanged by the bind';

# --- F: block-scope $ bind, shadowing caller is a BLOCK, not a routine -----
{
    my $fv = 1;
    my $fa;
    sub gf() { $fa := $fv }
    my $r;
    { my $fv = 5; gf(); $r = $fv }
    is $r, 5, 'F1: a shadowing BLOCK caller keeps its own binding';
    $fv = 9;
    is $fa, 9, 'F2: the alias tracks the block lexical';
    is $fv, 9, 'F3: the block lexical is unchanged by the bind';
}

# --- G: block-scope @ bind -------------------------------------------------
{
    my @gv = 1, 2, 3;
    my @ga;
    sub gg() { @ga := @gv }
    sub fg() { my @gv = 5, 6; gg(); @gv.elems }
    is fg(), 2, 'G1: @-sigil, the caller keeps its own array';
    is @ga.elems, 3, 'G2: the alias names the block array';
    is @gv.elems, 3, 'G3: the block array is unchanged';
}

# --- H: block-scope % bind -------------------------------------------------
{
    my %hv = a => 1;
    my %ha;
    sub gh() { %ha := %hv }
    sub fh() { my %hv = b => 2, c => 3; gh(); %hv.elems }
    is fh(), 2, 'H1: %-sigil, the caller keeps its own hash';
    is %ha.elems, 1, 'H2: the alias names the block hash';
    is %hv.elems, 1, 'H3: the block hash is unchanged';
}

# --- I: dynamic variable control - MUST keep caller priority ---------------
my $*idyn = 'outer';
sub gi() { $*idyn }
sub fi() { my $*idyn = 'caller'; gi() }
is fi(), 'caller', 'I1: a dynamic variable still resolves against the caller';
is $*idyn, 'outer', 'I2: the outer dynamic binding is restored';

# --- J: block-scope @ / % READ ---------------------------------------------
{
    my @jv = 1, 2, 3;
    my %jh = a => 1, b => 2, c => 3;
    sub gj() { @jv.elems }
    sub gk1() { %jh.elems }
    sub fj() { my @jv = 5, 6; gj() }
    sub fk() { my %jh = x => 9; gk1() }
    is fj(), 3, 'J1: block-scope @ read is lexical';
    is fk(), 3, 'J2: block-scope % read is lexical';
}

# --- K: FILE-SCOPE @ / % read control --------------------------------------
my @kv = 1, 2, 3;
my %kh = a => 1, b => 2, c => 3;
sub gk2() { @kv.elems }
sub gk3() { %kh.elems }
sub fk1() { my @kv = 5, 6; gk2() }
sub fk2() { my %kh = x => 9; gk3() }
is fk1(), 3, 'K1: file-scope @ read control';
is fk2(), 3, 'K2: file-scope % read control';

# --- L: FILE-SCOPE $ write control -----------------------------------------
my $lv = 1;
sub gl() { $lv = 3 }
sub fl() { my $lv = 5; gl(); $lv }
is fl(), 5, 'L1: file-scope write control';
is $lv, 3, 'L2: file-scope write lands on the file lexical';

# --- M: block-scope $ read / write with a shadowing BLOCK caller -----------
{
    my $mv = 1;
    sub gm() { $mv }
    sub gm2() { $mv = 3 }
    my ($r1, $r2);
    { my $mv = 5; $r1 = gm(); }
    { my $mv = 5; gm2(); $r2 = $mv; }
    is $r1, 1, 'M1: read through a shadowing block caller is lexical';
    is $r2, 5, 'M2: the shadowing block caller keeps its own value';
    is $mv, 3, 'M3: the write reached the block lexical';
}

# --- N: liveness - a later write in the declaring block is seen ------------
{
    my $nv = 1;
    sub gn() { $nv }
    is gn(), 1, 'N1: the capture is live, not a snapshot (before)';
    $nv = 2;
    is gn(), 2, 'N2: the capture is live, not a snapshot (after)';
}

# --- O: a closure created in the shadow block keeps its own capture --------
{
    my $ov = 'outer';
    sub go(&cb) { cb() }
    my $r;
    { my $ov = 'inner'; $r = go({ $ov }) }
    is $r, 'inner', 'O1: an argument closure keeps its own captured binding';
}

# --- P: two sibling blocks, same name, one sub each ------------------------
{
    my $pv = 'one';
    sub gp1() { $pv }
    is gp1(), 'one', 'P1: first sibling block';
}
{
    my $pv = 'two';
    sub gp2() { $pv }
    is gp2(), 'two', 'P2: second sibling block is a distinct binding';
}

# --- Q: nested bare blocks, depth 2 ----------------------------------------
{
    my $qv = 1;
    {
        my $qw = 2;
        sub gq() { $qv + $qw }
        sub fq() { my $qv = 50; my $qw = 60; gq() }
        is fq(), 3, 'Q1: both nesting depths resolve lexically';
        $qw = 20;
        is gq(), 21, 'Q2: liveness at depth 2';
    }
}

# --- R: a sub declared inside a ROUTINE ------------------------------------
sub outer-r() {
    my $rv = 1;
    sub gr() { $rv }
    return gr();
}
is outer-r(), 1, 'R1: a sub declared inside a routine still reads its lexical';

# --- S: a closure created INSIDE the block-scope sub -----------------------
{
    my $sv = 'outer';
    sub gs(&cb) { cb() }
    sub gs2() { (1,).map({ $sv }).list[0] }
    sub fs() { my $sv = 'caller'; gs2() }
    my $r1;
    { my $sv = 'inner'; $r1 = gs({ $sv }) }
    is $r1, 'inner', 'S1: the argument closure wins over the sub body';
    is fs(), 'outer', 'S2: a .map closure made inside the sub captures the cell';
}

# --- T: a for-loop body declaring a sub over the loop lexical -------------
my @tout;
for 1, 2 -> $i {
    my $tv = $i * 10;
    sub gt() { $tv }
    @tout.push(gt());
}
is @tout.join(','), '10,20', 'T1: a per-iteration lexical is re-captured';
