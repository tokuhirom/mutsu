use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `C .= new` on a type object is the typed `.=` node: `.AST` shows the dotty
# infix over the type, as on rakudo 2026.09, and the statement dies like the
# parsed program ("Cannot modify an immutable 'C' type object"), ADR-10723
# Stage 2, #14948. This file passes under both mutsu and raku.

plan 6;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $dotty = last-statement(Q[class C {}; C .= new]);
is $dotty.^name, 'RakuAST::ApplyDottyInfix', 'a type object .= is a dotty infix';
is $dotty.left.^name, 'RakuAST::Type::Simple', 'over the type';
is $dotty.right.name.canonicalize, 'new', 'applying the method';

throws-like { EVAL Q[class D {}; D .= new] }, X::Assignment::RO,
    message => "Cannot modify an immutable 'D' type object", 'the parsed program dies';
throws-like { EVAL Q[class E {}; E .= new].AST }, X::Assignment::RO,
    message => "Cannot modify an immutable 'E' type object", 'its tree dies the same way';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[class F {}; F .= new].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
