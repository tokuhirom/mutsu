use Test;

plan 36;

# ADR-0048 Phase 3 (D3/D6): constructs whose body DOES take a signature, and
# what each of them supplies when it invokes that body. Every expectation here
# was verified against real `raku` (the "Constructs whose body may take a
# signature" table in
# docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md), not
# against mutsu's previous output -- including the exact failure text, which
# rakudo reports as a plain X::AdHoc.
#
# The shared rule: a `{ ... }` body's placeholders are its own positional
# parameters, so supplying fewer arguments than it declares is the ordinary
# runtime arity failure -- `Too few positionals passed; expected N argument(s)
# but got M` -- raised when (and only when) the body is actually invoked.

# Assert the *exact* message, not just that it dies: the whole point of D3 is
# replacing two ad-hoc strings with raku's own wording.
sub arity-fails($code, $expected, $desc) {
    my $msg = 'did not die';
    try {
        EVAL $code;
        CATCH { default { $msg = .message } }
    }
    is $msg, $expected, $desc;
}

my $expect1 = 'Too few positionals passed; expected 1 argument but got 0';
my $expect-two-of-one = 'Too few positionals passed; expected 2 arguments but got 1';
my $expect-two-of-zero = 'Too few positionals passed; expected 2 arguments but got 0';

# --- if / unless / with / without supply ONE argument: the raw condition ---

is (if 42 { "$^a" }), '42', 'if supplies the raw condition to one placeholder';

arity-fails 'if 42 { "$^a $^b" }', $expect-two-of-one,
    'if with two placeholders is an arity failure (only the condition is supplied)';

arity-fails 'unless 0 { "$^a $^b" }', $expect-two-of-one,
    'unless with two placeholders is an arity failure';

arity-fails 'with 7 { "$^a $^b" }', $expect-two-of-one,
    'with, two placeholders: an arity failure';

arity-fails 'without Nil { "$^a $^b" }', $expect-two-of-one,
    'without with two placeholders is an arity failure';

arity-fails 'if 42 { "$^a"; my @x = @^b; }', $expect-two-of-one,
    'an @^b counts as a positional parameter too, not only $^a';

# The failure is a RUNTIME failure of the body's invocation, so a branch that
# never runs never raises it.
{
    my $ran = 0;
    if 0 { $ran = "$^a $^b" }
    is $ran, 0, 'a never-taken if branch does not raise its arity failure';
}

{
    my $ran = 0;
    if 1 { $ran = 1 } else { $ran = "$^a $^b" }
    is $ran, 1, 'a never-taken else branch does not raise either';
}

# Value position (`do if ...`, and an `if` as a routine's tail statement) goes
# through the same shared emitter.
is (do if 42 { "$^a" }), '42', 'value-position if supplies the raw condition';

arity-fails 'my $r = do if 42 { "$^a $^b" }', $expect-two-of-one,
    'value-position if with two placeholders is an arity failure';

sub tail-if { if 9 { "$^a" } }
is tail-if(), '9', 'tail-position if supplies the raw condition';

arity-fails 'sub f { if 42 { "$^a $^b" } }; f()', $expect-two-of-one,
    'tail-position if with two placeholders is an arity failure';

# --- given / with supply ONE argument: the topic ---

is (do given 5 { "$^a" }), '5', 'given supplies the topic to one placeholder';

arity-fails 'given 5 { "$^a $^b" }', $expect-two-of-one,
    'given with two placeholders is an arity failure (only the topic is supplied)';

arity-fails 'my $r = do given 5 { "$^a $^b" }', $expect-two-of-one,
    'value-position given with two placeholders is an arity failure';

# --- when supplies ZERO arguments ---

arity-fails 'given 5 { when 5 { $^c } }', $expect1,
    'a when body is invoked with no arguments, so one placeholder under-supplies it';

arity-fails 'given 5 { when 5 { "$^a $^b" } }', $expect-two-of-zero,
    'two placeholders in a when body report expected 2 but got 0';

{
    my $ran = 'no match';
    given 5 { when 6 { $ran = $^c }; }
    is $ran, 'no match', 'a non-matching when never invokes its body, so it never raises';
}

is { when 5 { $^c } }.arity, 0,
    'a when body is a boundary: its placeholder is not the enclosing block parameter';

# --- a bare `{ ... }` STATEMENT supplies ZERO arguments ---

arity-fails '{ $^c }', $expect1,
    'a bare block statement is invoked with no arguments';

arity-fails '{ "$^a $^b" }', $expect-two-of-zero,
    'two placeholders in a bare block report expected 2 but got 0';

arity-fails '{ $^c }; my $after = 1', $expect1,
    'a NON-tail bare block statement raises too (it used to leak its placeholder)';

arity-fails 'sub f { { $^c }; 99 }; f()', $expect1,
    'a bare block nested in a sub does not contribute to that sub signature';

arity-fails 'sub f { { $^c } }; f()', $expect1,
    'the same holds when the bare block is the sub tail statement';

# The `{ ... }` TERM in value position is a different construct: it is a real
# closure whose placeholders ARE its signature.
is { $^c }.arity, 1, 'a block TERM keeps its placeholder as its own parameter';
is { $^c }(7), 7, 'and binds it when invoked with an argument';

# --- statement modifiers introduce no block, so they bind nothing ---

sub modifier-nontail { my $seen; $seen = "$^a" if 1; $seen }
is modifier-nontail(7), '7',
    'a NON-tail if statement modifier leaves its placeholder to the enclosing routine';

sub modifier-tail { "$^a" if 1 }
is modifier-tail(7), '7', 'and so does a tail-position one';

sub modifier-two { "$^a $^b" if 1 }
is modifier-two(1, 2), '1 2',
    'a modifier never raises the arity failure of a block it does not have';

# --- constructs the enclosing body must still see through ---

# `for` supplies N elements per iteration, so N placeholders are all bound.
{
    my @out;
    for 1, 2 { @out.push("$^a $^b") }
    is @out.join('|'), '1 2', 'for supplies one element per placeholder';
}

# `repeat {} while/until` is signature-capable (ADR-0048 D4), so a placeholder
# inside one belongs to the repeat body -- NOT to an enclosing bare block, which
# would otherwise report it as an unsupplied parameter of that block. This is
# the shape roast/S04-statements/repeat.t pins.
{
    my $b = 1;
    my $tracker;
    repeat while $b < 10 {
        $tracker = $^a;
        $b++;
    }
    ok $tracker.defined, 'a placeholder in a repeat body does not leak to an enclosing bare block';
}

# --- a statement MODIFIER whose statement is a bare block ---
#
# The modifier introduces no block, but the statement it modifies can BE one,
# and then that block is the construct's own: raku supplies the modifier's
# value to it. `{ $a = $^x } unless 0` prints 0, not an arity failure -- so the
# zero-argument rule above must NOT fire for these.

{
    my $a; { $a = $^x } unless 0;
    is $a, 0, 'an unless modifier supplies its raw condition to a bare block statement';
}

{
    my $a; { $a = $^x } if 5;
    is $a, 5, 'an if modifier does the same';
}

{
    my $a; { $a = $^x } with 7;
    is $a, 7, 'a with modifier supplies its topic';
}

{
    my $a; { $a = $^x } given 69;
    is $a, 69, 'a given modifier supplies its topic';
}

sub tail-modifier-block { { $^x } unless 0 }
is tail-modifier-block(), 0, 'the same in a routine tail position';

# (A `while` modifier over a bare block is deliberately NOT pinned here: raku
# never calls the block at all, so `{ $n++ } while $n < 2` loops forever there
# while mutsu runs it. That divergence is D4/Phase 4's, not D3's -- Phase 3 only
# has to make sure the zero-argument arity check does not fire on such a block.)
