use v6;
use experimental :rakuast;
use Test;

# An optional RakuAST field a node does NOT carry still answers (#8124).
#
# A node only stores the fields its source produced, but raku declares every
# field as an attribute, so asking for an absent one is legal and yields an
# undefined value of the field's declared type. That is what makes `.defined`
# the natural way to test for an optional clause — without it, the only way to
# ask "does this statement have a loop modifier?" is to match `.gist` against a
# string, which is what `t/rakuast/rakuast-with-without-modifier.t` had to do.
#
# Measured against Rakudo 2026.07; every assertion passes under BOTH mutsu and
# raku.

plan 27;

# --- an absent node-typed field is that field's type object -----------------

my $with-mod = Q[say 1 with 2].AST.statements[0];
is $with-mod.loop-modifier.^name, 'RakuAST::StatementModifier::Loop',
    'an absent loop-modifier answers with its declared type object';
nok $with-mod.loop-modifier.defined,
    'and that type object is undefined, so .defined is the test for the slot';
ok $with-mod.condition-modifier.defined,
    'while the slot the with modifier DID fill is defined';

my $plain = Q[say 1].AST.statements[0];
is $plain.condition-modifier.^name, 'RakuAST::StatementModifier::Condition',
    'an absent condition-modifier answers with its own declared type object';
nok $plain.condition-modifier.defined, 'absent condition-modifier is undefined';
nok $plain.loop-modifier.defined, 'absent loop-modifier is undefined';

# The same holds across the conditional family the `with` block forms joined.
my $if = Q[if 1 { 2 }].AST.statements[0];
is $if.else.^name, 'RakuAST::Block', 'an absent else answers with (Block)';
nok $if.else.defined, 'an if with no else has an undefined else';
ok Q[if 1 { 2 } else { 3 }].AST.statements[0].else.defined,
    'an if that HAS an else answers with a defined Block';

my $with = Q[with 1 { 2 }].AST.statements[0];
nok $with.else.defined, 'a with with no else has an undefined else';
ok Q[with 1 { 2 } else { 3 }].AST.statements[0].else.defined,
    'a with that HAS an else answers with a defined Block';

# --- an absent List-typed field is an empty list, never undefined -----------

ok $if.elsifs.defined, 'an absent elsifs list is defined, not a type object';
is $if.elsifs.elems, 0, 'an absent elsifs list is empty';
is Q[if 1 { 2 } elsif 3 { 4 }].AST.statements[0].elsifs.elems, 1,
    'and a chain that has one reports it';
ok $with.elsifs.defined, 'the same holds for a with with no orwith clauses';
is $with.elsifs.elems, 0, 'an absent orwith chain is an empty list';

# --- an absent declaration/initializer slot ---------------------------------

my $decl = Q[my $x].AST.statements[0].expression;
is $decl.initializer.^name, 'RakuAST::Initializer',
    'an uninitialised declaration answers with the Initializer type object';
nok $decl.initializer.defined, 'and it is undefined';
ok Q[my $x = 1].AST.statements[0].expression.initializer.defined,
    'an initialised declaration answers with a defined Initializer';

# --- a field whose EMPTY value the renderer elides is still there -----------
# `sub f { }` gists with no `signature`, but the signature is not absent: it is
# an empty one, and rakudo answers with it.

my $sub = Q[sub f { }].AST.statements[0].expression;
ok $sub.signature.defined,
    'a sub written without a signature still has one';
is $sub.signature.^name, 'RakuAST::Signature', 'and it is a Signature';
is $sub.signature.parameters.elems, 0, 'with no parameters';
nok $sub.signature.returns.defined, 'and no return constraint';

# --- every statement modifier exposes the expression it was written with ----

is Q[say 1 with 2].AST.statements[0].condition-modifier.expression.^name,
    'RakuAST::IntLiteral', 'StatementModifier::With exposes .expression';
is Q[say 1 if 2].AST.statements[0].condition-modifier.expression.^name,
    'RakuAST::IntLiteral', 'StatementModifier::If exposes .expression';
is Q[say 1 unless 2].AST.statements[0].condition-modifier.expression.^name,
    'RakuAST::IntLiteral', 'StatementModifier::Unless exposes .expression';
is Q[(say 1 if $_.defined) given 2].AST.statements[0].loop-modifier.expression.^name,
    'RakuAST::IntLiteral', 'StatementModifier::Given exposes .expression';
