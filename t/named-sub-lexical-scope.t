use Test;

# ADR-0024: a mainline named sub's free-variable read/write resolves
# LEXICALLY (against the binding visible at the sub's declaration site),
# not DYNAMICALLY (against whatever the calling frame's env currently
# holds). Any block (`{ }`, `for`, `start { }`, ...) that declares a
# same-named `my` must not shadow the sub's true lexical binding.
#
# See docs/adr/0024-mainline-lexicals-for-named-subs.md, whose "full
# divergence matrix" table (raku-verified) is the acceptance test here — each
# row below is named after its table row. Every sub is declared at true
# MAINLINE scope (block_scope_depth 0, NOT inside an extra `{ }` wrapper): the
# capture that fixes this bug is gated on that depth, so wrapping a row's
# `my`/`sub` pair in its own scoping block (to isolate row-to-row variable
# names, which looks tempting) would silently turn every row back into the
# legacy dynamic-resolution path and defeat the point of the test. Per-row
# isolation instead comes from giving each row's variables/subs unique names.
# Only the actual SHADOWING block each row's shape requires stays a real `{ }`.

plan 10;

# Row 1: live mutation — a later plain reassignment of the captured lexical
# must still be visible (boxing must not freeze a stale snapshot).
my $row1_a = 1;
sub row1-fa { $row1_a }
$row1_a = 2;
is row1-fa(), 2, "row 1: live mutation of a captured mainline lexical";

# Row 2a/2b: a sub that WRITES its free var, called while a block shadows the
# name, must write the REAL outer lexical — not the shadow, and not lose the
# write once the shadow's scope ends.
my $row2_client = "outer";
sub row2-setter($v) { $row2_client = $v; }
{
    my $row2_client = "inner";
    row2-setter("set");
    is $row2_client, "inner",
        "row 2a: the shadow's own value is untouched by the sub's write";
}
is $row2_client, "set",
    "row 2b: the write reached the real outer lexical, not the shadow";

# Row 3: a closure CREATED INSIDE the sub (not just a bare free-var read)
# must also resolve through the sub's own captured cell when the sub is
# called while a block shadows the name.
my $row3_y = "outer";
sub row3-fy() {
    my @r = (1).map({ $row3_y });
    return @r[0];
}
{
    my $row3_y = "inner";
    is row3-fy(), "outer",
        "row 3: a closure made inside the sub captures the sub's own lexical";
}

# Row 4: a call made AFTER the shadowing block has exited must still see the
# real outer lexical (this already worked pre-fix, by coincidence — keep it
# passing as a regression guard).
my $row4_client = "outer";
sub row4-helper($u) { $row4_client }
{
    my $row4_client = "inner";
    row4-helper(0);
}
is row4-helper(0), "outer", "row 4: call after the shadowing block exited";

# Row 5: two subs (setter + getter) sharing one captured lexical must share
# ONE cell, not each get their own snapshot.
my $row5_v;
sub row5-set($x) { $row5_v = $x }
sub row5-get() { $row5_v }
row5-set(42);
is row5-get(), 42, "row 5: setter/getter share one captured mainline lexical";

# Row 6: a for-loop parameter that shadows the name must not leak into a sub
# called from the loop body (the shape that broke Cro's request routing).
my $row6_x = "outer";
sub row6-read() { $row6_x }
my $seen6;
for ("loopval",) -> $row6_x {
    $seen6 = row6-read();
}
is $seen6, "outer", "row 6: a for-loop param shadow does not leak into the sub";

# Row 7: same shape as row 6, but the sub is called from inside `start { }`
# (cross-thread) — the captured cell must stay live across the thread clone.
my $row7_x = "outer";
sub row7-read() { $row7_x }
my $seen7;
for ("loopval",) -> $row7_x {
    await start { $seen7 = row7-read(); }
}
is $seen7, "outer", "row 7: a for-loop param shadow does not leak across start{}";

# Row 8: a call from a block nested INSIDE the shadowing block must still see
# the real outer lexical (the predicate walks only the last routine frame,
# not an innermost-named-frame search, so nested caller blocks must not
# matter).
my $row8_client = "outer";
sub row8-helper() { $row8_client }
my $seen8;
{
    my $row8_client = "inner";
    {
        $seen8 = row8-helper();
    }
}
is $seen8, "outer", "row 8: call from a block nested inside the shadowing block";

# Row adv: the trap for a naive fix — a closure created IN the shadow block,
# then passed to and invoked BY the sub, must keep reading its OWN captured
# (shadowed) binding. The sub's mainline-lexical mechanism must not override
# a closure's own free-variable capture.
my $rowadv_y = "outer";
sub rowadv-invoke(&c) { c() }
{
    my $rowadv_y = "inner";
    my $c = { $rowadv_y };
    is rowadv-invoke($c), "inner",
        "row adv: a closure captured in the shadow block keeps its own binding";
}
