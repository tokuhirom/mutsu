use Test;

# A nested source `{ ... }` block is a block literal its enclosing block
# re-clones on every execution, so its own `state` restarts each time. A block
# carrying an ENTER/LEAVE/KEEP/UNDO phaser must run through a real `BlockScope`
# (inlining would drop the phasers), and that path used to skip the
# `ResetStateLocals` bracket its phaser-free sibling emits -- so the same block
# kept its `state` across iterations in VALUE position. See GitHub issue #7632.

plan 8;

# 1-2. Value position: the collecting `do for` tail. This is the shape that
#      diverged; the phaser-free control next to it always worked.
my @phaser-value = do for ^2 { { LEAVE { }; state $q = 0; $q++ } };
is @phaser-value.join(','), '0,0', 'a phaser block in value position re-clones its state';

my @plain-value = do for ^2 { { state $q = 0; $q++ } };
is @plain-value.join(','), '0,0', 'the phaser-free control still re-clones its state';

# 3-4. Statement position, both flavours.
my @phaser-stmt;
for ^2 { { LEAVE { }; state $q = 0; @phaser-stmt.push($q++) } }
is @phaser-stmt.join(','), '0,0', 'a phaser block in statement position re-clones its state';

my @plain-stmt;
for ^2 { { state $q = 0; @plain-stmt.push($q++) } }
is @plain-stmt.join(','), '0,0', 'the phaser-free statement control still re-clones its state';

# 5. ENTER, not just LEAVE.
my @entered = do for ^2 { { ENTER { }; state $q = 0; $q++ } };
is @entered.join(','), '0,0', 'an ENTER phaser block re-clones its state too';

# 6. A routine tail already behaved; keep it pinned.
sub h { { LEAVE { }; state $q = 0; $q++ } }
is "{h()},{h()}", '0,0', 'a phaser block in a routine tail re-clones its state';

# The deliberate PERSIST cases, which must NOT gain a reset.

# 7. The loop body's OWN `state` (no nested block) persists across iterations.
my @loop-own;
for ^3 { state $p = 0; @loop-own.push($p++) }
is @loop-own.join(','), '0,1,2', "a loop body's own state persists across iterations";

# 8. A postfix statement modifier introduces no block, so its `state` persists.
my @modifier;
for ^3 { state $n = 0; $n++ if True; @modifier.push($n) }
is @modifier.join(','), '1,2,3', 'a statement modifier does not restart state';
