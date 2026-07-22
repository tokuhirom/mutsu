use Test;

# A bare `when`/`default` in a sub/method body topicalizes on the routine: a
# matching `when` must return from the routine WITHOUT leaking its match state
# to an enclosing `given`/`with`. Regression: the global `when_matched` flag set
# inside the routine leaked out, so the enclosing given/with broke early and
# skipped the rest of its statements (e.g. `given $x { $r = f(); say $r }` never
# ran the say). Fixed by save/reset/restore of `when_matched` at the routine
# call boundary. Discovered via vCard::Parser t/03-actions (its content-line
# action calls `made-value`, a method with bare when/default, from inside a
# `with $topic { ... }` block).

plan 12;

sub classify($_) {
    when * > 10 { "big" }
    default { "small" }
}

# Routine value is still correct at top level (no enclosing topicalizer).
is classify(50), 'big',   'sub bare-when match value at top level';
is classify(3),  'small', 'sub bare-when default value at top level';

# Inside `given`, statements after the routine call must still run.
my $out = '';
given 'ctx' {
    $out = classify(20);
    $out ~= '-after';
}
is $out, 'big-after', 'sub bare-when in given does not skip the rest of the block';

# Inside `with`, likewise.
class C {
    method kind($_) {
        when Int { "int" }
        default { "other" }
    }
}
my $r = '';
with 'topic' {
    $r = C.new.kind(5);
    $r ~= '!';
}
is $r, 'int!', 'method bare-when in with does not skip the rest of the block';

# The topic assignment itself must complete (the earlier bug skipped it, so the
# var kept its pre-call value).
my $assigned = 'none';
given 'topic' {
    $assigned = classify(99);
}
is $assigned, 'big', 'assignment from a when-routine call inside given completes';

# Multiple when-routine calls in one given: every effect is visible.
sub tag($_) {
    when * %% 2 { 'even' }
    default { 'odd' }
}
my @res;
given 'x' {
    @res.push(tag(4));
    @res.push(tag(7));
    @res.push('end');
}
is @res.join(','), 'even,odd,end', 'multiple when-routines in one given all run';

# A `given` nested INSIDE a routine still works (the routine reset must not
# disturb the routine's own inner given).
sub describe($x) {
    my $s = 'none';
    given $x {
        when 1 { $s = 'one' }
        when 2 { $s = 'two' }
        default { $s = 'many' }
    }
    return $s;
}
is describe(1), 'one',  'given inside routine: first when';
is describe(2), 'two',  'given inside routine: second when';
is describe(9), 'many', 'given inside routine: default';

# `for` is also a topicalizer that inspects when_matched between iterations —
# a when-routine call in the loop body must not truncate the loop.
sub score($_) {
    when * >= 5 { 'hi' }
    default { 'lo' }
}
my @scores;
for 3, 7, 1 {
    @scores.push(score($_));
}
is @scores.join(','), 'lo,hi,lo', 'when-routine call inside for loop does not truncate iteration';

# Directly returning the caller's when state: an outer given whose own when
# matched, then calls a when-routine, must keep behaving correctly afterwards.
my $trace = '';
given 5 {
    when * > 0 {
        $trace ~= 'outer-';
        $trace ~= classify(1);   # inner routine: 'small'
        $trace ~= '-end';
    }
}
is $trace, 'outer-small-end', 'inner when-routine does not abort the outer when block';

# Nested routine calls: a when-routine that itself calls another when-routine.
sub outer($_) {
    when * > 0 { 'pos:' ~ classify($_ * 100) }
    default { 'nonpos' }
}
is outer(1), 'pos:big', 'nested when-routine calls compose correctly';
