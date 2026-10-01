use Test;

plan 5;

grammar B1 {
    token TOP { <t> }
    token t { a <.acc> }
    method acc { self }
}
grammar B2 is B1 { }
grammar B3 is B1 { token TOP { <t> <t> } }

ok B1.parse("a").so, 'a method called as a subrule on its own grammar';
ok B2.parse("a").so, 'a method inherited from a parent grammar is a subrule';
ok B3.parse("aa").so, 'an inherited method resolves in a derived grammar with its own TOP';

role R { method acc { self } }
grammar C1 { token TOP { <t> }; token t { a <.acc> } }
grammar C2 is C1 does R { }
ok C2.parse("a").so, 'a method composed from a role into the derived grammar';

my @log;
grammar D1 { token TOP { <t> }; token t { a <.acc> }; method acc { @log.push('D1'); self } }
grammar D2 is D1 { method acc { @log.push('D2'); self } }
D1.parse("a");
D2.parse("a");
is @log.join(','), 'D1,D2', 'a method overriding the parent one is the one called';
