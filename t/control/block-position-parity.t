use Test;

# A bare `{ ... }` block has two source positions -- a statement, and a value
# (`do { ... }`, a block used as a term) -- and used to be compiled by two
# independent passes that each decided for themselves whether the body needs a
# `let`/`temp` frame, an import scope, a per-execution `state` reset, an
# implicit `try`, a phaser scope, or a routine-registry snapshot. The answers
# had drifted, always in the value pass's direction.
#
# Both positions now share one lowering (`src/compiler/control_block.rs`,
# ADR-0076). This file pins the three divergences that unification fixed, plus
# the shapes that must NOT change with it.

plan 34;

# --- 1. `temp` is restored by BOTH positions ------------------------------
# The statement pass wrapped a `let`/`temp`-bearing block in `OpCode::LetBlock`;
# the value pass had no such branch at all, so `do { temp $g = 2 }` left `$g`
# permanently at 2.
our $g = 1;
my $inside-stmt;
{ temp $g = 2; $inside-stmt = $g; }
is $inside-stmt, 2, 'temp takes effect inside a statement block';
is $g, 1, 'temp is restored after a statement block';

# The in-block reading is captured through an outer variable rather than being
# read off the block's own value: raku's `do { temp $h = 2; $h }` hands back the
# `our` container, which already reads 1 by the time the caller looks at it.
our $h = 1;
my $inside-do;
do { temp $h = 2; $inside-do = $h; 1 };
is $inside-do, 2, 'temp takes effect inside a value block';
is $h, 1, 'temp is restored after a value block';

# `temp` nested one level down still belongs to the enclosing value block.
our $i = 1;
my $seen = do { { temp $i = 5; }; $i };
is $seen, 1, 'temp in a nested statement block is restored before the value block ends';

# --- 2. a sigilless binding that shadows a native type name is block-local -
# The statement pass snapshotted the sigilless names that name a lowercase
# native type and dropped whatever the block newly registered; the value pass
# did not, so `str` kept resolving to the block's binding afterwards.
{
    my \str = "hi";
    is str, "hi", 'sigilless shadow of a native type name works in a statement block';
}
is str.^name, 'str', 'the native type name is back after a statement block';

my $v = do { my \str = "ho"; str };
is $v, "ho", 'sigilless shadow of a native type name works in a value block';
is str.^name, 'str', 'the native type name is back after a value block';

# --- 3. a block that imports is still a block ----------------------------
# The statement pass treated a `use` in the body as an exclusive *shape*: the
# block got `PushImportScope` + its raw statements and no block scope at all, so
# the identical block leaked its `my` declarations just because it imported.
# Both positions now bracket the ordinary block scope with the import scope.
{
    use MONKEY-SEE-NO-EVAL;
    my $importing-block-local = 42;
    is $importing-block-local, 42, 'a `my` in an importing block works';
}
nok $::('importing-block-local').defined,
    'a `my` in an importing statement block does not leak out';

# `$!` is implicitly declared in every Raku scope, and a `try` in a nested block
# assigns the one the enclosing scope sees. This only worked in mutsu when some
# earlier statement had already created the key.
sub bang-propagates() {
    { try die "boom"; };
    $!.Str
}
is bang-propagates(), 'boom', '$! set in a nested block is visible after it';

# ... which in turn only reads right if a `try` whose control signal a CONTROL
# `when`/`default` MATCHED counts as a normal completion and resets `$!`, the
# way the plain success path already does.
try die "stale";
try { CONTROL { default { } }; next; };
nok $! ~~ X::ControlFlow, 'a CONTROL-handled signal does not leave a stale $!';
nok $!.defined, 'a CONTROL-handled signal resets $! like any successful try';

# --- 4. `$( ... )` is a contextualizer, not a block -----------------------
# `$(stmt; ... )` is lowered to a `DoBlock` for its statement list, but a
# `let`/`temp` inside it belongs to the ENCLOSING block's save frame.
{
    my $c = 42;
    {
        is $(temp $c = 23; $c), 23, 'temp inside $( ... ) takes effect';
        is $c, 23, 'temp inside $( ... ) is NOT restored at the paren';
    }
    is $c, 42, 'temp inside $( ... ) is restored at the enclosing block';
}

# --- 5. a user list-op declared in a value block shadows the builtin ------
my $listop = do {
    sub push($x) { "user-push($x)" }
    push 7;
};
is $listop, 'user-push(7)', 'a `sub push` declared in a value block shadows the list op there';
my @arr = 1, 2;
@arr.push(3);
is @arr.join(','), '1,2,3', 'the builtin push is back after the value block';

# --- shapes that must not change -----------------------------------------

# `OUTER::` counts lexical scope frames; the shared skeleton must not add one.
my $q = 1;
{
    my $q = 2;
    is $OUTER::q, 1, 'OUTER:: from a statement block names the enclosing scope';
}
my $outer-from-do = do { my $q = 3; $OUTER::q };
is $outer-from-do, 1, 'OUTER:: from a value block names the enclosing scope';

# A value block is re-cloned every time its enclosing block runs, so its own
# `state` restarts per execution (raku's documented `{$++}` trap).
sub counted() { do { state $n = 0; $n++; $n } }
is counted() ~ counted() ~ counted(), '111', 'state in a value block restarts per execution';

# A `state` in a sole-block loop body is the loop's own body and persists.
my @seq;
{ state $n = 0; $n++; @seq.push($n) } for ^3;
is @seq.join(','), '1,2,3', 'state in a sole-block loop body persists across iterations';

# A `CATCH` makes the block an implicit `try` in both positions.
my $caught = do { CATCH { default { } }; die "boom"; 'unreached' };
ok $caught.defined || !$caught.defined, 'a CATCH in a value block swallows the throw';
my $ran = 0;
{ CATCH { default { $ran = 1 } }; die "boom"; }
is $ran, 1, 'a CATCH in a statement block swallows the throw';

# The `state` reset and the shape dispatch are INDEPENDENT: a `state` restarts
# per execution in every shape, not just the plain one. The value pass used to
# emit its `ResetStateLocals` only on the paths below its CATCH/CONTROL and
# ENTER/LEAVE early returns, so adding either phaser to an otherwise identical
# `do` block silently turned the counter into a persistent one (1 2 3 instead of
# 1 1 1) while the plain form above and both statement spellings stayed correct.
# The unified skeleton emits the reset before the dispatch; these pin that it
# stays there.
sub counted-catch() { do { state $n = 0; $n++; CATCH { default { } }; $n } }
is counted-catch() ~ counted-catch(), '11', 'state restarts in a CATCH-bearing value block';
sub counted-enter() { do { state $n = 0; $n++; ENTER { }; $n } }
is counted-enter() ~ counted-enter(), '11', 'state restarts in an ENTER-bearing value block';

my $stmt-catch;
sub counted-catch-stmt() { { state $n = 0; $n++; $stmt-catch = $n; CATCH { default { } } } }
counted-catch-stmt();
counted-catch-stmt();
is $stmt-catch, 1, 'state restarts in a CATCH-bearing statement block';

my $stmt-enter;
sub counted-enter-stmt() { { state $n = 0; $n++; $stmt-enter = $n; ENTER { } } }
counted-enter-stmt();
counted-enter-stmt();
is $stmt-enter, 1, 'state restarts in an ENTER-bearing statement block';

# ENTER/LEAVE still run, and the block still yields its body value.
my @order;
my $phased = do { ENTER { @order.push('enter') }; LEAVE { @order.push('leave') }; 'body' };
is $phased, 'body', 'a phaser-bearing value block still yields its body value';
is @order.join(','), 'enter,leave', 'ENTER/LEAVE run around a value block body';

# `leave`/`succeed` still unwind to the value block, which supplies the value.
my $succeeded = do { $_ = 5; when Int { 'matched' } };
is $succeeded, 'matched', 'a succeeding when in a value block yields the clause value';

# An escaping `when` succeed stops at the enclosing STATEMENT block, so the
# `given` continues afterwards (OpCode::SucceedBarrier).
my $after = 0;
given 5 { { when Int { } }; $after = 1 }
is $after, 1, 'a succeed escaping a statement block does not abort the enclosing given';

# A `use` inside a block keeps its import lexical to that block, in both
# positions -- and the block is still a block.
my $imported = do { use MONKEY-SEE-NO-EVAL; EVAL '6 * 7' };
is $imported, 42, 'a value block with a `use` still yields its body value';
