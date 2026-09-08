use Test;

# A control construct written in expression position must be lowered by the
# same compiler pass as the statement form. These pin the divergences that the
# duplicated value-position pass used to have (see src/compiler/control_for.rs
# and src/compiler/control_if.rs).

plan 10;

# 1-3: loop phasers run in a value-position `for`. The old value-position
# lowering called `expand_loop_phasers` and then threw the FIRST/LAST
# statements away, so neither phaser ever fired.
my @seen;
my @doubled = do for 1, 2, 3 {
    FIRST { @seen.push: 'first' }
    LAST  { @seen.push: 'last' }
    $_ * 2
};
is-deeply @doubled, [2, 4, 6], 'value-position for collects its body values';
is @seen[0], 'first', 'FIRST phaser fires in a value-position for';
is @seen[*-1], 'last', 'LAST phaser fires in a value-position for';

# 4-5: an `is rw` (`<->`) loop over a `.reverse`d source writes back through the
# reversed container tag. The value-position lowering used to emit the plain
# `TagContainerRef`, writing each element back to the mirrored index.
my @nums = 1, 2, 3;
my @out = do for @nums.reverse <-> $x { $x = $x + 10; $x };
is-deeply @out, [13, 12, 11], 'value-position rw for over .reverse yields reversed values';
is-deeply @nums, [11, 12, 13], 'value-position rw for over .reverse writes back in source order';

# 6: a bare regex as the condition of a value-position `if` smartmatches the
# topic, exactly as the statement form does. The value path used to compile the
# regex as a plain (always-true) literal.
my $matched = do given 'abc' { do if /b/ { 'yes' } else { 'no' } };
is $matched, 'yes', 'bare regex condition in a value-position if matches the topic';
my $unmatched = do given 'abc' { do if /z/ { 'yes' } else { 'no' } };
is $unmatched, 'no', 'a non-matching bare regex condition takes the else branch';

# 7-8: multi-param binds are marked read-only in value position too.
my @pairs = do for (1, 2) -> $a, $b { $a + $b };
is-deeply @pairs, [3], 'value-position for binds a multi-param signature';
dies-ok { EVAL 'my @z = do for (1, 2) -> $a, $b { $a = 9 }' },
    'a value-position for parameter is read-only, like the statement form';

# 9: `.reverse`-free rw writeback still works (guards the tag choice above).
my @plain = 1, 2, 3;
my @plain-out = do for @plain <-> $x { $x = $x * 2; $x };
is-deeply @plain-out, [2, 4, 6], 'value-position rw for over a plain array writes back';
