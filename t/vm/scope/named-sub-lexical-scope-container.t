use Test;

# ADR-0039 slice 1: the `@`/`%` container half of ADR-0024's mainline
# lexical-scope fix (`t/named-sub-lexical-scope.t` is the scalar original —
# see its header comment for the full rationale, mirrored here). A mainline
# named sub's `@`/`%` free variable must resolve LEXICALLY (against the
# binding visible at the sub's declaration site), not DYNAMICALLY (against
# whatever the calling frame's `env` currently holds) — an ordinary block
# that shadows the name with its own `my @x`/`my %h` must not hijack a
# sub's mutation (`push`, element-assign, key-set, `:delete`) of the real
# outer container.
#
# Every sub below is declared at true MAINLINE scope (block_scope_depth 0,
# not inside an extra `{ }` wrapper) for the same reason
# `named-sub-lexical-scope.t` requires it: the capture is gated on that
# depth. Per-row isolation comes from unique row-specific variable/sub
# names, not from wrapping a row in its own block.

plan 8;

# Row 1: live mutation via `push` — a sub's push to the mainline `@a` must be
# visible through the same binding afterwards (boxing must not freeze a
# stale snapshot).
my @row1_a = (1, 2);
sub row1-push() { @row1_a.push(3) }
row1-push();
is @row1_a.join(","), "1,2,3", "row 1: live push through a captured mainline @array";

# Row 2a/2b: a sub that pushes to its free `@array` var, called while a block
# shadows the name, must push onto the REAL outer array — not the shadow,
# and the outer array must show the push once the shadow's scope ends.
my @row2_client = <a b>;
sub row2-pusher($v) { @row2_client.push($v) }
{
    my @row2_client = <x y z>;
    row2-pusher("c");
    is @row2_client.join(","), "x,y,z",
        "row 2a: the shadow's own array is untouched by the sub's push";
}
is @row2_client.join(","), "a,b,c",
    "row 2b: the push reached the real outer array, not the shadow";

# Row 3: a closure CREATED INSIDE the sub (not just a bare free-var read)
# must also resolve through the sub's own captured cell when the sub is
# called while a block shadows the name.
my @row3_y = <outer1 outer2>;
sub row3-fy() {
    my @r = (1).map({ @row3_y.join(",") });
    return @r[0];
}
{
    my @row3_y = <inner1 inner2>;
    is row3-fy(), "outer1,outer2",
        "row 3: a closure made inside the sub captures the sub's own array";
}

# Row 5: two subs (pusher + reader) sharing one captured `@array` must share
# ONE container, not each get their own snapshot.
my @row5_v;
sub row5-push($x) { @row5_v.push($x) }
sub row5-read() { @row5_v.join(",") }
row5-push(42);
row5-push(43);
is row5-read(), "42,43", "row 5: pusher/reader share one captured mainline @array";

# Row 6: a for-loop parameter that shadows the name must not leak into a sub
# called from the loop body.
my @row6_x = <outer>;
sub row6-read() { @row6_x.join(",") }
my $seen6;
for ((<loopval>,),) -> @row6_x {
    $seen6 = row6-read();
}
is $seen6, "outer", "row 6: a for-loop @param shadow does not leak into the sub";

# Row 8: a call from a block nested INSIDE the shadowing block must still see
# the real outer array.
my @row8_client = <outer>;
sub row8-helper() { @row8_client.join(",") }
my $seen8;
{
    my @row8_client = <inner>;
    {
        $seen8 = row8-helper();
    }
}
is $seen8, "outer", "row 8: call from a block nested inside the shadowing @block";

# Row hash: the `%h` twin of row 2 — a sub that key-sets its free `%hash` var,
# called while a block shadows the name, must set on the REAL outer hash.
my %rowh_client = (a => 1);
sub rowh-setter($k, $v) { %rowh_client{$k} = $v }
{
    my %rowh_client = (x => 10);
    rowh-setter("y", 20);
    is %rowh_client.sort(*.key).map({ "{.key}={.value}" }).join(","), "x=10",
        "row hash: the shadow's own hash is untouched by the sub's key-set";
}
