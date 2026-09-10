use Test;

# Slice F (env<->locals coherence): an *exceptional* exit (`die`/`fail`) still
# runs a routine's UNDO/LEAVE phasers, which can mutate a captured-outer lexical
# (e.g. `UNDO { $ng ~= "U" }`). The body records those captured-outer writes into
# `pending_rw_writeback_sources`, but the call-site op used `?` to propagate the
# error, short-circuiting the drain that writes the new env value through to the
# caller's local slot. With the reverse env->locals pull disabled
# (MUTSU_NO_REVERSE_SYNC=1) the caller's slot then stayed stale. This pins the
# exception-escape drain: the call site now drains pending writeback sources
# before propagating the error, exactly as the success path does.

plan 12;

# --- UNDO after die, value caught by an outer `try` -------------------------
my $ng1 = "";
sub fails1() {
    UNDO { $ng1 ~= "U"; }
    die "boom";
}
try { fails1(); }
is $ng1, "U", 'UNDO captured-outer write after die writes through';

# --- UNDO after fail --------------------------------------------------------
my $ng2 = "";
sub fails2() {
    UNDO { $ng2 ~= "F"; }
    fail "nope";
}
try { fails2(); }
is $ng2, "F", 'UNDO captured-outer write after fail writes through';

# --- LEAVE (runs on every exit, including exceptional) ----------------------
my $log3 = "";
sub leaver3() {
    LEAVE { $log3 ~= "L"; }
    die "x";
}
try { leaver3(); }
is $log3, "L", 'LEAVE captured-outer write after die writes through';

# --- Compound mutation (+=) in UNDO ----------------------------------------
my $count4 = 0;
sub fails4() {
    UNDO { $count4 += 5; }
    die "x";
}
try { fails4(); }
is $count4, 5, 'UNDO compound += writes through';

# --- UNDO fires twice across two failing calls (accumulation) ---------------
my $acc5 = 0;
sub fails5() {
    UNDO { $acc5++; }
    die "x";
}
try { fails5(); }
try { fails5(); }
is $acc5, 2, 'UNDO accumulation across two failing calls writes through';

# --- Captured-outer string append in UNDO ----------------------------------
my $trail6 = "";
sub fails6() {
    UNDO { $trail6 ~= "a"; }
    die "x";
}
try { fails6(); }
try { fails6(); }
is $trail6, "aa", 'UNDO string append accumulation writes through';

# --- Both KEEP (skipped on failure) and UNDO (run) --------------------------
my $kept7 = 0;
my $undone7 = 0;
sub fails7() {
    KEEP { $kept7++; }
    UNDO { $undone7++; }
    die "x";
}
try { fails7(); }
is $kept7, 0, 'KEEP does not run on failure (caller slot stays 0)';
is $undone7, 1, 'UNDO runs on failure and writes through';

# --- KEEP on SUCCESS still writes through (control) -------------------------
my $kept8 = 0;
sub ok8() {
    KEEP { $kept8++; }
    42;
}
ok8();
is $kept8, 1, 'KEEP captured-outer write on success writes through';

# --- LEAVE on success still writes through (control) ------------------------
my $log9 = "";
sub ok9() {
    LEAVE { $log9 ~= "L"; }
    1;
}
ok9();
is $log9, "L", 'LEAVE captured-outer write on success writes through';

# --- die directly (no phaser) leaves outer write before die visible ---------
my $pre10 = 0;
sub fails10() {
    $pre10 = 7;
    die "x";
}
try { fails10(); }
is $pre10, 7, 'captured-outer write before die writes through on exception';

# --- Multiple captured vars mutated in UNDO ---------------------------------
my $a11 = 0;
my $b11 = 0;
sub fails11() {
    UNDO { $a11 = 1; $b11 = 2; }
    die "x";
}
try { fails11(); }
is "$a11,$b11", "1,2", 'multiple captured-outer writes in UNDO write through';
