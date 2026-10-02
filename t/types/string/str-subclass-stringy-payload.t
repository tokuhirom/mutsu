use Test;

# #11026: a `Str` subclass already is a `Str`. `Str.Stringy` is `self`, and
# the native string operators read the payload, so a subclass's own `Str`
# is reached only through prefix `~` and `.Str`.

plan 21;

class MyStr is Str { method Str { "strr" } }
my $s = MyStr.new(value => "q");

is "{$s}", 'q', 'interpolation of a block uses the payload';
is "a $s b", 'a q b', 'interpolation of a variable uses the payload';
is $s ~ "!", 'q!', 'infix ~ uses the payload';
is ~$s, 'strr', 'prefix ~ calls the subclass Str';
is $s.Str, 'strr', '.Str calls the subclass Str';
isa-ok $s.Stringy, MyStr, '.Stringy is the instance itself';
is $s.Stringy.raku, '"q"', '... which renders as its payload';
ok $s eq 'q', 'eq compares the payload';

class OwnStringy is Str { method Stringy { "own" } }
my $t = OwnStringy.new(value => "v");
is "$t", 'own', 'interpolation calls a declared Stringy';
is ~$t, 'v', 'prefix ~ without a declared Str is the payload';
is $t ~ "!", 'v!', 'infix ~ uses the payload past a declared Stringy';
ok $t eq 'v', 'eq uses the payload past a declared Stringy';

class Both is Str { method Str { "s" }; method Stringy { "own" } }
my $u = Both.new(value => "v");
is ~$u, 's', 'prefix ~ prefers the declared Str';
is "$u", 'own', 'interpolation prefers the declared Stringy';
is $u ~ 1, 'v1', 'infix ~ still uses the payload';
is join("-", $u, $u), 'v-v', 'join() uses the payload';
is ($u, $u).join(","), 'v,v', '.join uses the payload';
is ($u, $u).Str, 'v v', 'List.Str uses the payload';
is "{($u, $u)}", 'v v', 'interpolated list uses the payload';
is ([~] $u, $u), 'vv', '[~] uses the payload';

class Plain is Str {}
is "<{Plain.new(value => "z")}>", '<z>', 'a subclass without stringifiers interpolates its payload';
