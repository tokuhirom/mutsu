use Test;

# `Allomorph` declares `ACCEPTS`, `WHICH` and `raku` about the allomorph itself;
# they are rows of `Allomorph` reached with the allomorph as the invocant
# (ADR-11276 slice 5). `gist` is `Str`'s, on the string part.

plan 14;

is <1>.WHICH, 'IntStr|Int|1|Str|1', 'IntStr.WHICH names both parts';
is <1.5>.WHICH, 'RatStr|Rat|3/2|Str|1.5', 'RatStr.WHICH';
is <1e0>.WHICH, 'NumStr|Num|1|Str|1e0', 'NumStr.WHICH';
ok <42>.WHICH eqv <42>.WHICH, 'equal allomorphs have one identity';

is <1>.raku, 'IntStr.new(1, "1")', 'IntStr.raku';
is <1e3>.raku, 'NumStr.new(1000e0, "1e3")', 'NumStr.raku';

ok <1>.ACCEPTS(1), 'a number is matched with the number';
ok <1>.ACCEPTS("1"), 'a string is matched with the string';
nok <01>.ACCEPTS("1"), '... not with the number';
ok <1>.ACCEPTS(1.0), 'a Rat is matched numerically';
ok "42" ~~ <42>, 'smartmatch against an allomorph';

class C { method Str { "1" }; method Numeric { 1 } }
ok <1>.ACCEPTS(C.new), 'an object is matched through its .Str and .Numeric';

is <1e3>.gist, '1e3', 'gist is the source string';
is [<1e3>, <2>].gist, '[1e3 2]', '... also inside a list';
