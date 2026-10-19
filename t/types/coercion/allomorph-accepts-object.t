use Test;

# `Allomorph.ACCEPTS` matches a `Numeric` argument with the number, a `Str`
# with the string, and any other object with both, through the object's own
# `.Str` and `.Numeric` (ADR-11276 §9.191; every answer checked against
# Rakudo).

plan 15;

class N { method Numeric { 42 }; method Str { "42" } }
class M { method Numeric { 42 }; method Str { "x" } }
class NR does Real { method Bridge { 42e0 } }
class S is Str { }

ok <42>.ACCEPTS(N.new), 'an object whose .Str and .Numeric both match';
nok <42>.ACCEPTS(M.new), 'an object whose .Str does not';
nok <4.2e1>.ACCEPTS(N.new), 'a NumStr compares its own string';
ok N.new ~~ <42>, 'smartmatch with an allomorph matcher';
nok M.new ~~ <42>, '... that does not match';
ok <42>.ACCEPTS(NR.new), 'a Numeric object is matched by the number';
ok <42>.ACCEPTS(S.new(value => "42")), 'a Str object is matched by the string';
nok <42>.ACCEPTS(S.new(value => "x")), '... that does not match';
nok <42> ~~ N.new, 'the object as the matcher is identity';

ok S.new(value => "42") ~~ "42", 'a Str subclass instance smartmatches its string';
nok S.new(value => "4") ~~ "42", '... and not another one';

ok <42>.ACCEPTS("42") && <42>.ACCEPTS(42), 'a Str and an Int argument';
ok <42>.ACCEPTS(<42>), 'an allomorph argument';

class C { method Numeric { 3+5i }; method Str { "3+5i" } }
ok (3+5i).ACCEPTS(C.new), 'a Complex matches an object through its .Numeric';
ok < 3+5i>.ACCEPTS(C.new), 'and so does a ComplexStr';
