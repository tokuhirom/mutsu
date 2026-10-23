use Test;

plan 5;

# An enclosing `use fatal` does not reach a role body; one written inside the
# body does. A class method declared under `use fatal` stays fatal (#11452).
use fatal;

role R { method m { my $x = "a".Int; 1 } }
class C does R {}
role P[$t] { method m { my $x = "a".Int; 1 } }
class CP does P[1] {}
role F { use fatal; method m { my $x = "a".Int; 1 } }
class CF does F {}
class K { method m { my $x = "a".Int; 1 } }

is (try { C.m; "lived" } // "died"), "lived", 'role method does not inherit the outer use fatal';
is (try { CP.m; "lived" } // "died"), "lived", 'parametric role method does not inherit it either';
is (try { CF.m; "lived" } // "died"), "died", 'use fatal inside the role body applies';
is (try { K.m; "lived" } // "died"), "died", 'class method under use fatal is fatal';
is (try { C.m; "lived" } // "died"), "lived", 'repeat call is stable';
