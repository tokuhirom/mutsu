use v6;
use Test;

plan 11;

# A program that declares `multi prefix:<->` / `prefix:<!>` / `postfix:<!>` must
# not stop `-*.height` / `!*.closed` from currying into a WhateverCode: the
# operator is parsed as a call of its routine, but its operand still primes.
class AST { }
class T { has $.height; has $.closed }
multi prefix:<->(AST $a) { "ast-neg" }
multi prefix:<!>(AST $a) { "ast-not" }
multi postfix:<!>(AST $a) { "ast-fact" }

my @t = T.new(:height(3), :closed(False)), T.new(:height(1), :closed(True));

is @t.sort(-*.height).map(*.height).join(","), "3,1", "-*.method sorts descending";
is @t.map(!*.closed).join(","), "True,False", "!*.method maps";
is (-*.height).WHAT.^name, "WhateverCode", "-*.method is a WhateverCode";
is (!*.closed).WHAT.^name, "WhateverCode", "!*.method is a WhateverCode";
my $neg = -*.height;
is $neg.arity, 1, "and has arity 1";
is (-*.height)(T.new(:height(7))), -7, "calling it applies the built-in negation";
is (!*.closed)(T.new(:closed(True))), False, "calling the not form applies the built-in not";

# The user candidates still take their own types.
is -AST.new, "ast-neg", "the user prefix:<-> still dispatches on its type";
is !AST.new, "ast-not", "the user prefix:<!> still dispatches on its type";

# A user-declared postfix curries its operand too.
my &f = *.height!;
is &f.WHAT.^name, "WhateverCode", "a postfix operator on *.method is a WhateverCode";

# An ordinary call's arguments never curry.
sub takes-whatever($x) { $x.WHAT.^name }
is takes-whatever(*), "Whatever", "a bare * passed to a sub stays a Whatever value";
