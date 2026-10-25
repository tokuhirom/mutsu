use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# `temp` / `let` is a prefix operator over an lvalue (RakuAST's ApplyPrefix),
# with the assignment around it. The parser keeps that shape and lowers it to
# the save the compiler runs.

plan 14;

sub expression-of(Str $source) { ('our $x; our $y; ' ~ $source).AST.statements[2].expression }

is expression-of('temp $x').^name, 'RakuAST::ApplyPrefix', 'a bare temp is a prefix';
is expression-of('temp $x = 1').^name, 'RakuAST::ApplyInfix', 'an assignment wraps the prefix';
is expression-of('let $x = 1').left.prefix.^name, 'RakuAST::Prefix', 'let is a prefix too';
is expression-of('temp $x ~= "a"').^name, 'RakuAST::ApplyInfix', 'a compound assignment wraps it';

is EVAL('our $x = 1; sub f { temp $x = 2; $x }; f() ~ $x'.AST), '21', 'temp restores at scope exit';
is EVAL('our @a = 1, 2; sub f { temp @a = 3, 4; @a.join }; f() ~ @a.join'.AST), '3412',
    'temp of an array';
is EVAL('our %h = a => 1; sub f { temp %h<a> = 5; %h<a> }; f() ~ %h<a>'.AST), '51',
    'temp of a hash element';
is EVAL('our @a = 1, 2, 3; sub f { temp @a[1] = 9; @a.join }; f() ~ @a.join'.AST), '193123',
    'temp of an array element';
is EVAL('our $s = "a"; sub f { temp $s ~= "b"; $s }; f() ~ $s'.AST), 'aba',
    'temp with a compound assignment';
is EVAL('my %h = a => {b => 1}; sub f { temp %h<a><b> = 2; %h<a><b> }; f() ~ %h<a><b>'.AST), '21',
    'temp of a nested element';
is EVAL('our $y = 1; sub f { temp $y = 2 if True; $y }; f() ~ $y'.AST), '21',
    'temp under a statement modifier';
is EVAL('our $z = 1; sub f { temp $z = 2 if False; $z }; f()'.AST), 1,
    'a false modifier does not save or assign';
is EVAL('our $w = 1; sub f { { let $w = 2; Nil } }; f().raku ~ $w'.AST), 'Nil1',
    'let restores when the block fails';
is EVAL('our $v = 1; sub f { { let $v = 2; $v } }; f() ~ $v'.AST), '22',
    'let keeps the value when the block succeeds';
