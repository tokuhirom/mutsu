use v6;
use Test;
use experimental :rakuast;

# A symbolic lookup qualified by a written package, `PKG::(EXPR)`. Rakudo
# 2026.09 keeps the package parts and the key apart:
# `Name(Part::Simple("MY"), Part::Expression(EXPR))`. Under a lexical
# pseudo-package (MY, LEXICAL, OUR, CORE, CALLER(S), OUTER(S), DYNAMIC, ...)
# the name is a `Term::Name`; under GLOBAL, PROCESS, EXPORT or a user package
# it is a `Call::Name` over the same name.

plan 15;

sub expr(Str $source) { $source.AST.statements[*-1].expression }

# --- read direction ---------------------------------------------------------

my $my = expr(Q[my $x; MY::(q[$x])]);
is $my.^name, 'RakuAST::Term::Name', '`MY::(…)` is a Term::Name';
is $my.name.parts.elems, 2, 'its name has two parts';
is $my.name.parts[0].^name, 'RakuAST::Name::Part::Simple', 'the package is a simple part';
is $my.name.parts[0].name, 'MY', 'naming the package';
is $my.name.parts[1].^name, 'RakuAST::Name::Part::Expression', 'the key is an expression part';

is expr(Q[CALLERS::(q[$_])]).^name, 'RakuAST::Term::Name', '`CALLERS::(…)` is a Term::Name';
my $global = expr(Q[GLOBAL::('Int')]);
is $global.^name, 'RakuAST::Call::Name', '`GLOBAL::(…)` is a Call::Name';
is $global.name.parts[0].name, 'GLOBAL', 'over the same name shape';
is expr(Q[MY::Foo::('x')]).name.parts.elems, 3, 'a qualified package keeps each segment';

# --- write direction (EVAL) and semantics -----------------------------------

is Q[my $x = 42; MY::(q[$x])].AST.EVAL, 42, 'the round-tripped MY lookup reads the lexical';
is Q[my $x = 7; LEXICAL::('$' ~ 'x')].AST.EVAL, 7, 'a computed key';
our $rakuast-indirect-global = 17;
is Q[GLOBAL::(q[$rakuast-indirect-global])].AST.EVAL, 17, 'the GLOBAL lookup reads the global';
is Q[MY::('nope')].AST.EVAL.exception.^name, 'X::NoSuchSymbol',
    'a missing symbol is a Failure of X::NoSuchSymbol';

my $x = 42;
is MY::(q[$x]), 42, 'the ordinary frontend reads the lexical';
is GLOBAL::(q[$rakuast-indirect-global]), 17, 'and the global';
