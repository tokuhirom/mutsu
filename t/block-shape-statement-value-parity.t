use Test;

# A bare `{ ... }` block is compiled by two passes -- `Stmt::Block` for
# statement position and `compile_do_block_expr` for value position -- and both
# used to decide independently what kind of block they were looking at. This
# file pins the cases where the two answers disagreed; both passes now dispatch
# on the shared `BlockShape` classifier. See GH-7569 and
# docs/adr/0076-bare-block-keeps-two-opcodes-one-shape.md.

plan 12;

# --- per-execution `state` restart -------------------------------------------
# A block literal is re-cloned every time its ENCLOSING block runs, so its own
# `state` restarts per execution. The value form computed that reset only AFTER
# its CATCH / phaser early-returns, so adding a `CATCH` or an `ENTER` to an
# otherwise identical `do` block silently turned the counter into a persistent
# one.

sub plain-do() { do { state $n = 0; $n++; $n } }
is plain-do(), 1, 'do { state } restarts on the first call';
is plain-do(), 1, 'do { state } restarts on the second call';

sub catch-do() { do { state $n = 0; $n++; CATCH { default { } }; $n } }
is catch-do(), 1, 'do { state; CATCH } restarts on the first call';
is catch-do(), 1, 'do { state; CATCH } restarts on the second call';

sub enter-do() { do { state $n = 0; $n++; ENTER { }; $n } }
is enter-do(), 1, 'do { state; ENTER } restarts on the first call';
is enter-do(), 1, 'do { state; ENTER } restarts on the second call';

# The statement-position spellings always did the right thing; pin them so the
# shared classifier cannot regress that direction either.
my $stmt-catch;
sub catch-stmt() { { state $n = 0; $n++; $stmt-catch = $n; CATCH { default { } } } }
catch-stmt();
catch-stmt();
is $stmt-catch, 1, '{ state; CATCH } as a statement restarts per execution';

my $stmt-enter;
sub enter-stmt() { { state $n = 0; $n++; $stmt-enter = $n; ENTER { } } }
enter-stmt();
enter-stmt();
is $stmt-enter, 1, '{ state; ENTER } as a statement restarts per execution';

# --- `let`/`temp` stays a statement-position shape -------------------------
# `BlockShape::LetBlock` exists in the shared classifier but the value path
# declines it on purpose: `Expr::DoBlock` is not a Raku block -- item context
# (`$( ... )`), the chained-comparison desugar, `cas` and compound-assignment
# lowering all use the same node, and none of them may resolve a `let`. Pin the
# idiom roast relies on: the `let` inside `$( ... )` belongs to the enclosing
# block, so that block's exit is what restores it.
# (A genuine `do { let $x = 2; Nil }` does not roll back yet -- GH-7635.)

my $item-ctx = 42;
{
    is $(let $item-ctx = 23; $item-ctx), 23, 'let inside $( ) takes effect there';
    Mu;
}
is $item-ctx, 42, 'let inside $( ) is restored by the ENCLOSING block, not by $( )';

my @arr = (0, 1, 2);
{
    is $(let @arr[1] = 42; @arr[1]), 42, 'let on an array element inside $( )';
    Mu;
}
is @arr[1], 1, 'let on an array element is restored by the enclosing block';
