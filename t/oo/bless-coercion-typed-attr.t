use Test;

# From the SQL::Builder distribution: `self.bless(:$<relation>)` into a
# `has Str() $.relation` attribute must coerce the provided value (a Match)
# through the target type, and an unmatched capture (Nil) must reset the
# attribute to the `Str` type object, not the `Str()` coercion type.

plan 7;

class A {
    has Str() $.x;
    has Str() $.y;
    method new($s) {
        $s ~~ /$<x>=(\w+) ['-' $<y>=(\d+)]?/;
        self.bless(:$<x>, :$<y>);
    }
}

my $a = A.new("foo-12");
is $a.x.WHAT.gist, '(Str)', 'Match is coerced to Str by bless';
is $a.x, 'foo', 'coerced value';
is $a.y, '12', 'second attribute coerced';

my $b = A.new("foo");
ok !$b.y.defined, 'unmatched capture leaves the attribute undefined';
is $b.y.WHAT.gist, '(Str)', 'and it is the Str type object';

class B { has Int() $.i; }
is B.bless(i => "42").i.WHAT.gist, '(Int)', 'bless coerces Str to Int';
is B.bless(i => "42").i, 42, 'coerced Int value';

done-testing;
