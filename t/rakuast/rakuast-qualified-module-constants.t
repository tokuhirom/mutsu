use v6;
use lib 't/lib';
use Test;
use experimental :rakuast;

# An `our` constant a `use`d module declares is installed in the module's
# package, so its qualified name is a known term in the importer: rakudo
# 2026.09 renders `Pkg::NAME` as a `Term::Name`, and only an unknown name as a
# `Call::Name`. A `my` constant is lexical to the module and stays unknown.
use RakuASTQualifiedConstants;

plan 9;

# --- read direction ---------------------------------------------------------

# The `use` is part of each string, so the conversion's own parse loads the
# module and knows what it declares.
sub term-class(Str $name) {
    "use RakuASTQualifiedConstants; $name".AST.statements[1].expression
}
is term-class('RakuASTQualifiedConstants::ANSWER').^name,
    'RakuAST::Term::Name', 'a qualified `our` constant renders as a Term::Name';
is term-class('RakuASTQualifiedConstants::Inner::DEPTH').^name,
    'RakuAST::Term::Name', 'so does one declared in a nested package';
is term-class('RakuASTQualifiedConstants::HIDDEN').^name,
    'RakuAST::Call::Name', 'a `my` constant is not installed in the package';
is term-class('RakuASTQualifiedConstants::ANSWER').name.canonicalize,
    'RakuASTQualifiedConstants::ANSWER', 'the Term::Name keeps the qualified name';

# --- write direction (EVAL) and semantics -----------------------------------

is Q[use RakuASTQualifiedConstants; RakuASTQualifiedConstants::ANSWER].AST.EVAL, 42, 'the round-tripped term evaluates';
is Q[use RakuASTQualifiedConstants; RakuASTQualifiedConstants::Inner::DEPTH].AST.EVAL, 2, 'and the nested one';
is RakuAST::Term::Name.new(
    RakuAST::Name.from-identifier-parts('RakuASTQualifiedConstants', 'ANSWER')
).EVAL, 42, 'a hand-built Term::Name evaluates';

is RakuASTQualifiedConstants::ANSWER, 42, 'the ordinary frontend reads the constant';
is RakuASTQualifiedConstants::Inner::DEPTH, 2, 'and the nested one';
