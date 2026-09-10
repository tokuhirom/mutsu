use Test;

# ADR-0052 Slice 1: every construct that runs a statement range owns a stack
# base and returns to it, so a `when`/`default` that matches (and abandons the
# range via `succeed`) can never leave its body's value behind for an unrelated
# consumer to eat.
#
# The detector is a two-argument consumer: the clause's value travels in the
# succeed signal, so a duplicate copy left on the stack is silently taken in
# place of the sibling argument (`say "A: ", (given 2 { when 2 { "two" } })`
# printed "twotwo" instead of "A: two").

plan 16;

# Expression-position `given` (the ADR's named pin, §1.1(c)).
is join('|', 'A: ', (given 2 { when 2 { 'two' } })), 'A: |two',
    'matching when in an expression-position given does not eat its sibling argument';
is join('|', 'D: ', (given 3 { default { 'd' } })), 'D: |d',
    'matching default in an expression-position given does not eat its sibling argument';

# Loop bodies: each iteration must return to the loop's own stack base, whether
# or not the loop collects. A matching `when` abandons the body mid-range on
# every pass, so a missing truncation leaked one value per matching iteration.
is join('|', 'L1: ', (do { for 1..3 { when 2 { 'x' } }; 'end' })), 'L1: |end',
    'for over an int range stays stack-neutral across a matching when';
is join('|', 'L2: ', (do { for <a b c> { when 'b' { 'x' } }; 'end' })), 'L2: |end',
    'for over a list stays stack-neutral across a matching when';
is join('|', 'L3: ', (do { $_ = 2; my $i = 0; while $i++ < 3 { when 2 { 'x' } }; 'end' })),
    'L3: |end',
    'while stays stack-neutral across a matching when';
is join('|', 'L4: ', (do { $_ = 2; loop (my $j = 0; $j < 3; $j++) { when 2 { 'x' } }; 'end' })),
    'L4: |end',
    'C-style loop stays stack-neutral across a matching when';
is join('|', 'L5: ', (do { $_ = 2; my $k = 0; repeat { when 2 { 'x' } } while ++$k < 3; 'end' })),
    'L5: |end',
    'repeat loop stays stack-neutral across a matching when';
is join('|', 'L6: ', (do { for 1..3 { default { 'x' } }; 'end' })), 'L6: |end',
    'a default in a loop body stays stack-neutral';

# A *collecting* loop body is compiled without the `SucceedBarrier` its
# sink-position twin gets, so the succeed reaches the loop op itself. When the
# matching iteration is the last one there is no following iteration to sweep
# up after it, and the abandoned body's value stayed on the stack underneath
# the array the loop pushes — the consumer then took the stray value and lost
# its sibling argument. These probes assert only that the sibling survives:
# what a matching iteration *collects* is ADR-0052 Slice 3's subject and is
# still wrong (`do for 1..3 { when 2 { 'hit' }; 'plain' }` has 3 elements in
# raku and 2 here), which is why they do not pin `.elems`.
ok join('|', 'C1: ', (do for 1..3 { when 3 { 'hit' } }).elems).starts-with('C1: |'),
    'a collecting for loop nets exactly one value when its last iteration matches';
ok join('|', 'C2: ', (do for gather { take 1; take 2; take 3 } { when 3 { 'hit' } }).elems)
        .starts-with('C2: |'),
    'the same for a collecting for over a lazy gather';
ok join('|', 'C3: ', (do for 1..3 { default { 'hit' } }).elems).starts-with('C3: |'),
    'the same when every iteration is abandoned by a default';
{
    $_ = 2;
    my $i = 0;
    ok join('|', 'C4: ', (do while $i++ < 3 { when 2 { 'hit' } }).elems).starts-with('C4: |'),
        'the same for a collecting while loop';
    $_ = 2;
    ok join('|', 'C5: ', (do loop (my $j = 0; $j < 3; $j++) { when 2 { 'hit' } }).elems)
            .starts-with('C5: |'),
        'the same for a collecting C-style loop';
    $_ = 2;
    my $k = 0;
    ok join('|', 'C6: ', (do repeat { when 2 { 'hit' } } while ++$k < 3).elems)
            .starts-with('C6: |'),
        'the same for a repeat loop';
}

# A CONTROL handler runs its own statement range too. A `when` matching inside
# it used to leave the handler body's value behind, which then became the
# enclosing block's value (raku yields an undefined value there).
my $c1 = do { last; CONTROL { when CX::Last { 7 } } };
nok $c1.defined, 'a block ended by a CONTROL-handled signal does not yield the handler body value';
my $c2 = do { warn 'w'; CONTROL { when CX::Warn { 7 } } };
nok $c2.defined, 'the same for a CONTROL-handled warn';
