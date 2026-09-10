use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `$x where EXPR` in the read direction (ADR-0011).
#
# `RakuAST::Parameter.new(:where)` has been constructible since Phase 4 slice 11
# (t/rakuast-construct-where.t), and `EVAL` lowers and *enforces* the constraint.
# Only the read direction was missing: a where-constrained parameter was
# refused outright as a "non-trivial signature parameter", so the one shape
# `.new` builds could not be obtained from source.
#
# Passes under BOTH mutsu and raku.

plan 12;

sub param-of($src) {
    $src.AST.statements[0].expression.signature.parameters[0]
}

# --- read side: the `where` field ------------------------------------------
my $p = param-of(Q{sub f($x where * > 0) { $x }});
ok $p.gist.contains('where'), 'a where-constrained parameter renders its where field';
ok $p.where.defined, 'the where accessor is reachable from a parsed parameter';

# --- the `*` in the constraint is a priming argument, not a value ----------
# ADR-0033's leaf table: `* > 0` is `WhateverCode::Argument`, not
# `Term::Whatever`. The classifier used to stop at a routine's body, so a `*` in
# a signature kept the value classification it was parsed with.
ok $p.gist.contains('RakuAST::WhateverCode::Argument'),
    'a `*` in a where constraint is a priming argument';
nok $p.gist.contains('RakuAST::Term::Whatever'),
    'a `*` in a where constraint is not the Whatever value';

# --- a block constraint renders as a Block ----------------------------------
ok param-of(Q{sub f($x where { $_ > 0 }) { $x }}).gist.contains('RakuAST::Block.new('),
    'a block where-constraint renders as a Block';

# --- a typed parameter keeps both fields ------------------------------------
my $typed = param-of(Q{sub f(Int $x where * > 0) { $x }});
ok $typed.gist.contains('RakuAST::Name.from-identifier("Int")')
    && $typed.gist.contains('where'),
    'a typed where-constrained parameter keeps its type and its constraint';

# --- an unconstrained parameter is unchanged --------------------------------
nok param-of(Q{sub f($x) { $x }}).gist.contains('where'),
    'a plain parameter emits no where field';

# --- write side: the constraint survives and is enforced --------------------
is EVAL(Q{sub f($x where * > 0) { $x }; f(5)}.AST), 5,
    'a where-constrained parameter accepts a passing argument';
throws-like { EVAL(Q{sub f($x where * > 0) { $x }; f(-5)}.AST) }, Exception,
    'a where-constrained parameter rejects a failing argument';
is EVAL(Q{sub f($x where { $_ > 0 }) { $x }; f(7)}.AST), 7,
    'a block where-constraint round-trips';
is EVAL(Q{sub f(Int $x where * > 0) { $x }; f(2)}.AST), 2,
    'a typed where-constrained parameter round-trips';

# --- the constraint composes with a thunk barrier ---------------------------
is EVAL(Q{sub f($x where * > 0 && * < 5) { $x }; f(3)}.AST), 3,
    'a where constraint spanning a `&&` round-trips';
