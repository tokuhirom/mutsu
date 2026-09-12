use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# The `with` / `without` / `orwith` BLOCK forms across the RakuAST boundary.
#
# raku models them as `Statement::With` / `::Without` / `::Orwith`, with the
# block carrying `implicit-topic` / `required-topic` — it is the *block* that
# topicalizes, not an enclosing `given`. mutsu's parser desugars the whole thing
# into `if (my $tmp = X).defined { given X { BODY } }`, which a hand-written
# conditional of the same shape also produces, so the internal AST carries a
# marker recording which keyword the source used rather than guessing from the
# shape (or from the synthetic temp's name, which a program may declare itself).
# Measured against Rakudo; every assertion below passes under BOTH mutsu and raku.

plan 25;

# --- read direction: the node classes ---------------------------------------

is Q[with 1 { say 2 }].AST.statements[0].^name, 'RakuAST::Statement::With',
    'the with block form renders as Statement::With';
is Q[without Nil { say 2 }].AST.statements[0].^name, 'RakuAST::Statement::Without',
    'the without block form renders as Statement::Without';
is Q[with 1 { say 2 }].AST.statements[0].condition.^name, 'RakuAST::IntLiteral',
    'the condition is the written expression, not the .defined test';
is Q[without Nil { say 2 }].AST.statements[0].condition.^name, 'RakuAST::Type::Simple',
    'without: the condition is the written expression too';

# `With` names its block `then`; `Without` names it `body` — the same asymmetry
# `Statement::If` and `Statement::Unless` have.
is Q[with 1 { say 2 }].AST.statements[0].then.^name, 'RakuAST::Block',
    'with: the block is the `then` slot';
is Q[without Nil { say 2 }].AST.statements[0].body.^name, 'RakuAST::Block',
    'without: the block is the `body` slot';

# The block itself topicalizes, with a *required* topic.
like Q[with 1 { say 2 }].AST.gist, /'implicit-topic => True'/,
    'the with block is marked implicit-topic';
like Q[with 1 { say 2 }].AST.gist, /'required-topic => 1'/,
    'the with block takes a required topic';

# No `given` survives the boundary: the topicalizer of the desugar is the
# block's own flag in raku, not a statement.
unlike Q[with 1 { say 2 }].AST.gist, /'Statement::Given'/,
    'the desugared topicalizer does not leak out as a given';

# --- else / orwith chains ---------------------------------------------------

is Q[with 1 { say 2 } else { say 3 }].AST.statements[0].else.^name, 'RakuAST::Block',
    'with: a trailing else fills the `else` slot';
is Q[with 1 { say 2 } orwith 2 { say 3 }].AST.statements[0].elsifs[0].^name,
    'RakuAST::Statement::Orwith',
    'an orwith clause renders as Statement::Orwith in `elsifs`';
is Q[with 1 { say 2 } elsif 0 { say 3 }].AST.statements[0].elsifs[0].^name,
    'RakuAST::Statement::Elsif',
    'an elsif clause after with is still an Elsif';
is Q[if 1 { say 1 } orwith 2 { say 2 }].AST.statements[0].elsifs[0].^name,
    'RakuAST::Statement::Orwith',
    'an orwith clause continuing a plain if is an Orwith too';

# An `else` continuing a topicalizing clause topicalizes; one continuing an
# `elsif` does not.
like Q[with 1 { say 2 } orwith 2 { say 3 } else { say 4 }].AST.gist,
    /'else' .* 'implicit-topic => True'/,
    'the else after an orwith topicalizes';
unlike Q[with 1 { say 2 } elsif 0 { say 3 } else { say 4 }].AST.statements[0].else.gist,
    /'implicit-topic'/,
    'the else after an elsif does not topicalize';

# --- a nested `with` statement is not a continuation clause ------------------

is Q[if 1 { say 1 } else { with 2 { say 2 } }].AST.statements[0].elsifs.elems, 0,
    'a with block inside an else is a statement, not an elsif clause';
like Q[if 1 { say 1 } else { with 2 { say 2 } }].AST.statements[0].else.gist,
    /'Statement::With'/,
    'that nested with block is converted inside the else block';

# --- write direction: EVAL of the round-tripped AST -------------------------

is EVAL(Q[with 42 { $_ }].AST), 42,
    'with: the block runs, topicalized, on a defined condition';
is EVAL(Q[my $n = 0; with Nil { $n = 1 }; $n].AST), 0,
    'with: the block is skipped on an undefined condition';
is EVAL(Q[without Nil { 7 }].AST), 7,
    'without: the block runs on an undefined condition';
is EVAL(Q[with Nil { 1 } else { $_.defined ?? 2 !! 3 }].AST), 3,
    'with: the else branch runs, topicalized on the tested value';
is EVAL(Q[with Nil { 1 } orwith 5 { $_ } else { 9 }].AST), 5,
    'with: an orwith clause runs, topicalized on its own condition';

# The condition is evaluated exactly once, and an lvalue condition stays
# writable through the topic.
is EVAL(Q[my $c = 0; sub bump() { $c++; 1 }; with bump() { }; $c].AST), 1,
    'with: the condition is evaluated exactly once';
is EVAL(Q[my $x = 5; with $x { $_++ }; $x].AST), 6,
    'with: a variable condition aliases the topic back to the source';

# --- semantics, from source, unchanged by the marker ------------------------

{
    my @seen;
    with Nil { @seen.push: 'with' }
    orwith 3 { @seen.push: "orwith:$_" }
    else     { @seen.push: 'else' }
    is @seen.join(','), 'orwith:3', 'source: the with/orwith/else chain picks the orwith clause';
}
