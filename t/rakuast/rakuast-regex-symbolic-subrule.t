use v6;
use Test;

# `<::(EXPR)>` calls the rule whose name EXPR computes. Measured on rakudo
# 2026.09, it is a Regex::Assertion::Named whose name is
# Name(Part::Empty, Part::Expression(EXPR)), with `capturing => True` unless it
# is written `<.::(EXPR)>`; an alias `<q=::(EXPR)>` wraps it in
# Regex::Assertion::Alias.

plan 11;

my $named = Q[/ <::("alpha")> /].AST.statements[0].expression.body;
$named = $named.regex if $named.^name eq 'RakuAST::Regex::WithWhitespace';
my $named-gist = $named.gist;
like $named-gist, /^ 'RakuAST::Regex::Assertion::Named.new('/, 'a symbolic call is an Assertion::Named';
like $named-gist, /'RakuAST::Name::Part::Empty.new,' \s+ 'RakuAST::Name::Part::Expression.new('/,
    'its name is an empty part and an expression part';
like $named-gist, /'capturing => True'/, 'it captures';

my $dot = Q[/ <.::("alpha")> /].AST.statements[0].expression.body.gist;
unlike $dot, /'capturing'/, 'a dotted symbolic call does not capture';

my $alias = Q[/ <q=::("alpha")> /].AST.statements[0].expression.body.gist;
like $alias, /'RakuAST::Regex::Assertion::Alias.new(' \s+ 'name' \s+ '=> "q",'/,
    'an aliased symbolic call is an Assertion::Alias';

# EVAL of the round-tripped trees matches as the parsed regexes do.
my $n = 'alpha';
my $rx = EVAL Q[/ <::($n)> /].AST;
ok 'a' ~~ $rx, 'EVAL of a computed call matches';
is ('a' ~~ EVAL Q[/ <q=::($n)> /].AST).hash.keys.join(','), 'q',
    'EVAL of an aliased call files the alias';
is ('a' ~~ EVAL Q[/ <.::($n)> /].AST).hash.elems, 0,
    'EVAL of a dotted call files nothing';
my $b = 'alpha';
ok 'a' ~~ EVAL(Q[/ <::("$b")> /].AST), 'an interpolated string name lowers back';

# Semantics of the parsed regexes.
grammar G { token TOP { <::("w")> }; token w { \w } }
is G.parse('x')<w>.Str, 'x', 'a grammar token calls a literal-named rule';
nok 'a' ~~ / <::('digit')> /, 'a computed call to a builtin rule fails on a mismatch';
