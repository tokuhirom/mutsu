use Test;

# `$o does SOMEVALUE` / `$o but SOMEVALUE` composes a role with one method,
# named after the value's type. A lexical class (`my class Bars`) is stored
# under an internal key; the method still answers to the name the program
# wrote (roast S03-operators/misc.t, "does works with non-roles").

plan 5;

my class Bars { }
my $m = Bars.new;
my $o = class { method Str { 'o' } }.new;
$o does $m;
ok $o.Bars === $m, 'does with an instance of a my class adds a method named after it';
is $o.Str, 'o', 'and keeps the object\'s own methods';

my $q = my class Inline { }.new;
my $p = class { }.new;
$p does $q;
ok $p.Inline === $q, 'a my class written as a term too';

my class E { }
my $t = class { }.new but E.new;
is $t.E.^name, 'E', 'but with an instance of a my class';

class Global { }
my $g = class { }.new but Global.new;
is $g.Global.^name, 'Global', 'a package-scoped class is unchanged';
