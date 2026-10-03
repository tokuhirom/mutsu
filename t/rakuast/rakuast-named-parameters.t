use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Named parameters across the RakuAST boundary (ADR-10723 Stage 1).
#
# Rakudo models every named parameter as one `RakuAST::Parameter` whose
# `names` list holds each name it binds under, innermost first: `:s(:$sort)`
# is `names => ("sort", "s")`, `:foo($bar)` is `names => ("foo",)` with the
# target `$bar`. Its type, default, `where` clause and `!`/`?` marker are that
# node's fields. An untyped `@`/`%`/`&` parameter carries no implicit
# `Type::Setting(Any)`. Measured on rakudo 2026.09.
#
# Passes under BOTH mutsu and raku.

plan 25;

sub params-of($src) {
    $src.AST.statements[0].expression.signature.parameters
}

# --- read side: names, innermost first --------------------------------------
my @p = params-of(Q{sub f(:$w, :s(:$sort), :a(:b(:$c)), :foo($bar), :x(:y($z))) { }});
is-deeply @p[0].names.List, ("w",), q{:$w binds under its own name};
is-deeply @p[1].names.List, ("sort", "s"), ':s(:$sort) lists the inner name first';
is-deeply @p[2].names.List, ("c", "b", "a"), 'a nested alias lists every name, innermost first';
is @p[2].target.name, '$c', 'the alias targets the innermost variable';
is-deeply @p[3].names.List, ("foo",), ':foo($bar) binds under the alias only';
is @p[3].target.name, '$bar', ':foo($bar) targets $bar';
is-deeply @p[4].names.List, ("y", "x"), ':x(:y($z)) lists both aliases';

# --- read side: the parameter's own fields -----------------------------------
@p = params-of(Q{sub f(Int :$a = 3, :$b where 1, Str:D :$c!, :$d?, :$e, Int :n(:$m) = 2) { }});
is @p[0].type.^name, 'RakuAST::Type::Simple', 'a typed named parameter keeps its type';
is @p[0].default.^name, 'RakuAST::IntLiteral', 'a defaulted named parameter keeps its default';
ok @p[1].where.defined, 'a where-constrained named parameter keeps its where clause';
is @p[2].type.^name, 'RakuAST::Type::Definedness', 'Str:D renders as Type::Definedness';
is @p[2].optional, False, 'a required named parameter has optional => False';
is @p[3].optional, True, 'a `?` named parameter has optional => True';
nok @p[4].optional.defined, 'a plain named parameter leaves optional unset';
is-deeply @p[5].names.List, ("m", "n"), 'an aliased typed parameter keeps both names';
is @p[5].type.^name, 'RakuAST::Type::Simple', '... and the type written on the outer level';

# --- read side: no implicit type on @ / % / & parameters ---------------------
@p = params-of(Q{sub f(@a, %h, &c, $s, :@n) { }});
nok @p[$_].type.defined, "an untyped {@p[$_].target.name} has no implicit type" for 0, 1, 2, 4;
is @p[3].type.^name, 'RakuAST::Type::Setting', 'an untyped $ parameter keeps Type::Setting(Any)';

# --- write side: EVAL of the round-tripped tree binds the same way -----------
my $f = EVAL Q{sub (Int :$a = 3, :s(:$sort), :foo($bar), :x(:y($z))!, Str:D :$c = "c") {
    "$a $sort $bar $z $c"
}}.AST;
is $f(:s(1), :foo(2), :y(3)), '3 1 2 3 c', 'aliases bind under the outer name';
is $f(:a(9), :sort(1), :foo(2), :x(3)), '9 1 2 3 c', '... and under the inner name';
dies-ok { $f(:s(1), :foo(2)) }, 'a required alias chain is still required';

my $g = EVAL Q{sub (:$p ($q, $r), Int() :$i = 1, Array[Int] :$ai) { "$q $r {$i + 1}" }}.AST;
is $g(:p((1, 2)), :i("4")), '1 2 5', 'named destructuring and a coercion type survive the round trip';
