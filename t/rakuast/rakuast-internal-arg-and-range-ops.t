use v6;
use experimental :rakuast;
use Test;

# Two `.AST` leaks of mutsu internals, both silent wrongness rather than
# coverage gaps, both measured against rakudo 2026.07.
#
#   1. mutsu's parser attaches a `__mutsu_test_callsite_line => N` named
#      argument to every listop call so a failing `Test` assertion can report the
#      caller's line. It is instrumentation, not something the source wrote, and
#      it was rendered as a real argument — on calls as ordinary as `f()`.
#
#   2. `token_kind_to_op_name` had no rows for the exclusive range operators, so
#      they fell to its `{:?}` fallback and `1..^3` rendered
#      `Infix.new("DotDotCaret")` — a Rust variant name, not an operator anyone
#      wrote.
#
# Fixing (1) exposed a third: raku omits `args` entirely for an argument-less
# call, so filtering the injected argument out left an empty `ArgList` where
# there should be no field at all.
#
# Passes under BOTH mutsu and raku.

plan 10;

# --- no injected argument leaks into a call ---------------------------------
my $call = Q{sub f { }; f()}.AST.gist;
nok $call.contains('__mutsu'), 'no mutsu-internal argument is rendered';
nok $call.contains('args'), 'an argument-less call omits its args field';

my $say = Q{say 42}.AST.gist;
nok $say.contains('__mutsu'), 'a listop call renders no internal argument';
ok $say.contains('RakuAST::IntLiteral.new(42)'), 'a listop keeps its real argument';

my $one = Q{sub f($x) { }; f(1)}.AST.gist;
ok $one.contains('args => RakuAST::ArgList.new('), 'a call with arguments keeps its args field';

# --- the exclusive range operators render their real spelling ---------------
for '..^', '^..', '^..^' -> $op {
    my $src = "my \@a = 1 $op 3";
    ok $src.AST.gist.contains("RakuAST::Infix.new(\"$op\")"),
        "`$op` renders as itself";
}

# --- the inclusive range is unchanged ----------------------------------------
ok Q{my @a = 1..3}.AST.gist.contains('RakuAST::Infix.new("..")'),
    '`..` still renders as itself';

# --- no Rust variant name leaks anywhere in a range gist ---------------------
nok Q{my @a = 1..^3}.AST.gist.contains('DotDot'),
    'no TokenKind variant name leaks into the rendered operator';
