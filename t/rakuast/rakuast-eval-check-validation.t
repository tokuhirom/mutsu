use lib 't/lib';
use Test;
use experimental :rakuast;

# `RakuAST::Node.EVAL` runs the same CHECK-time validation as a string EVAL
# (#13295), measured on rakudo 2026.09: an undeclared variable, name or routine
# in the tree is a compile-time error (rakudo reports a hand-built type name
# as `X::Undeclared`, mutsu as `X::Undeclared::Symbols`) before anything runs, and a second declaration of a package in one process is a
# redeclaration. Names the tree does declare or import still evaluate.

plan 12;

throws-like { Q[say $undeclared-xx].AST.EVAL }, X::Undeclared,
    'an undeclared variable';
throws-like {
    RakuAST::StatementList.new(RakuAST::Statement::Expression.new(
        expression => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('UndeclaredTypeXx')))).EVAL
}, Exception, message => /UndeclaredTypeXx/, 'an undeclared type name';
throws-like { Q[undeclared-routine-xx()].AST.EVAL }, X::Undeclared::Symbols,
    'an undeclared routine';

my $ran = 0;
throws-like { Q[$ran = 1; undeclared-routine-xx()].AST.EVAL }, X::Undeclared::Symbols,
    'a rejected tree ...';
is $ran, 0, '... runs none of its statements';

Q[class Redeclared13295 { }].AST.EVAL;
throws-like { Q[class Redeclared13295 { }].AST.EVAL }, X::Redeclaration,
    'a second declaration of the same class';

# Declared and imported names still evaluate.
is Q[constant term:<$bar> = 42; $bar + 1].AST.EVAL, 43, 'a sigiled term constant';
is Q[grammar G13295 { token value:sym<number> { \d+ }; token TOP { <value:sym<number>> } }; G13295.parse("12").Str].AST.EVAL,
    '12', 'a `:sym<...>` subrule adverb';
is Q[use EnumTaggedExportFixture :BareTag; C].AST.EVAL.key, 'C', 'a tag-exported enum value';
is Q[use ExportHookOtherTerm :u; other].AST.EVAL, 42, 'a term a `sub EXPORT` hook installs';
is Q[module N13295 { our $v is export(:t) = 3 }; import N13295 :t; $v].AST.EVAL, 3,
    'a variable an `import` brings in';
is Q[my $r = ((my $a, my $b) »=» (1, 2)); "$a $b"].AST.EVAL, '1 2',
    'declarations inside a hyper-assignment target';
