use Test;

plan 14;

# #10992: an `Int`/`Num` subclass's own `Str` is what interpolation and
# prefix `~` render. Both go through `.Stringy`, which `Numeric` defines as
# `self.Str`, so the subclass instance (not its numeric payload) answers it.

class MyInt is Int { method Str { "mine" } }
my $x = MyInt.new(3);
is "{$x}", 'mine', 'a lone interpolated Int subclass uses its own Str';
is ~$x, 'mine', 'prefix ~ uses the Int subclass Str';
is "a $x b", 'a mine b', 'interpolation among text uses the Int subclass Str';
is $x.Stringy, 'mine', '.Stringy calls the Int subclass Str';
is $x ~ '!', 'mine!', 'infix ~ uses the Int subclass Str';
is $x + 1, 4, 'the subclass is still numerically its payload';

class MyNum is Num { method Str { "numm" } }
my $n = MyNum.new(1.5e0);
is "{$n}", 'numm', 'a lone interpolated Num subclass uses its own Str';
is ~$n, 'numm', 'prefix ~ uses the Num subclass Str';
is "a $n b", 'a numm b', 'interpolation among text uses the Num subclass Str';

class PlainInt is Int { }
my $p = PlainInt.new(4);
is "{$p}", '4', 'a subclass without its own Str interpolates its payload';
is ~$p, '4', 'prefix ~ of a plain subclass is its payload';
is $p.Stringy, '4', '.Stringy of a plain subclass is its payload';
isa-ok $p.Stringy, Str, '.Stringy of a plain subclass is a Str';

class PlainNum is Num { }
is ~PlainNum.new(2.5e0), '2.5', 'prefix ~ of a plain Num subclass is its payload';
