use Test;

# A bareword naming a CORE setting type (`X::AdHoc`, `IO::Path`) across the
# RakuAST boundary (ADR-10723 Stage 1). rakudo resolves it against the
# setting at parse time and renders `Type::Simple` (measured on 2026.09),
# like any builtin type. This file passes under both mutsu and raku.

plan 7;

sub expr(Str $src) { $src.AST.statements[0].expression }

is expr(Q[X::AdHoc]).^name, 'RakuAST::Type::Simple', 'X::AdHoc is a Type::Simple';
like expr(Q[X::AdHoc]).gist, /'from-identifier-parts("X","AdHoc")'/,
    'with a qualified name';
is expr(Q[IO::Socket::Async]).^name, 'RakuAST::Type::Simple', 'a three-part name';
is expr(Q[Proc::Async.new("true")]).operand.^name, 'RakuAST::Type::Simple',
    'as a method-call invocant';

# Write direction.
is EVAL(Q[X::AdHoc.new(payload => "p").message].AST), 'p', 'X::AdHoc round-trips';
is EVAL(Q[IO::Path.new("a/b").basename].AST), 'b', 'IO::Path round-trips';
is EVAL(Q[try { die X::AdHoc.new(payload => "z") }; $!.^name].AST), 'X::AdHoc',
    'a thrown setting exception round-trips';
