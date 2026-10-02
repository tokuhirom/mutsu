use Test;

# A user subclass of `Rat` is constructed positionally like `Rat` itself
# (`R.new(numerator, denominator)`) and behaves as that Rat. Like `Int`/`Num`
# subclasses, it gists through a user `Str`; `.raku` renders as
# `<numerator/denominator>` (rakudo prints the decimal form only for a `Rat`
# itself). mutsu#11025.

plan 17;

class MyRat is Rat { method Str { "ratt" } }
my $r = MyRat.new(1, 2);
is $r.gist, 'ratt', '.gist goes through the user Str';
is "{$r}", 'ratt', 'and so does interpolation';
is $r + 1, 1.5, 'arithmetic uses the fraction';

class PR is Rat { }
my $p = PR.new(6, 8);
is PR.new(3, 4) + 0, 0.75, 'a plain subclass adds like a Rat';
is $p.numerator, 3, 'the fraction is reduced: numerator';
is $p.denominator, 4, 'and denominator';
is-deeply $p.nude, (3, 4), '.nude';
ok $p ~~ Rat, 'it is a Rat';
is $p.WHAT.^name, 'PR', 'of its own type';
is $p.Rat.^name, 'PR', '.Rat returns the invocant';
is $p * 2, 1.5, 'multiplication';
ok $p == 0.75, 'numeric equality';
is $p.raku, '<3/4>', '.raku is the fraction form';
is PR.new(4, 2).raku, '2.0', 'an integral value renders as n.0';
is PR.new.Numeric, 0, 'no arguments is 0/1';

class MI is Int { method Str { "ii" } }
is MI.new(3).gist, 'ii', 'an Int subclass gists through its user Str too';
class MN is Num { method Str { "nn" } }
is MN.new(2.5).raku, 'nne0', 'and a Num subclass .raku adds the exponent';
