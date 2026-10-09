use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# A typed attribute starts with its type object only when it is a scalar. A typed
# `@` or `%` attribute is an empty container of that element type, also after the
# declaration has been through its RakuAST tree.
plan 9;

is EVAL(Q[class C1 { has Int %.h }; C1.new.h.WHAT.^name].AST), 'Hash[Int]', 'a typed hash attribute is a Hash[Int]';
is EVAL(Q[class C2 { has Int @.a }; C2.new.a.WHAT.^name].AST), 'Array[Int]', 'a typed array attribute is an Array[Int]';
is EVAL(Q[class C3 { has Int @.a }; C3.new.a.elems].AST), 0, 'and starts empty';
is EVAL(Q[class C4 { has Int $.x }; C4.new.x.WHAT.^name].AST), 'Int', 'a typed scalar starts with its type object';
is EVAL(Q[class C5 { has Str:D @.e; method add($s) { @!e.push($s) } }; my $c = C5.new; $c.add("x"); $c.e.join(",")].AST), 'x',
    'a smiley on the element type of an array attribute does not reject its elements';
is EVAL(Q[class C6 { has Int $.x is required }; C6.new(x => 3).x].AST), 3, 'a required scalar takes its argument';
dies-ok { EVAL Q[class C7 { has Int $.x is required }; C7.new].AST }, 'and is required';
is EVAL(Q[class C8 { has Int $.x = 7 }; C8.new.x].AST), 7, 'an initializer wins over the type seed';
is EVAL(Q[class C9 { has Int %.h }; my $c = C9.new; $c.h<a> = 1; $c.h.keys.join(",")].AST), 'a', 'a typed hash attribute takes elements';
