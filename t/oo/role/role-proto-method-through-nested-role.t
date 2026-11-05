use Test;

# #16751 (found by SQL::Abstract): a `proto method` declared in a role keeps its
# body when the role reaches the class through another role (`role S does R`),
# and the body sees the role's own `sub` helpers.

plan 10;

role R {
	proto method render(Int $x --> Str) { my $r = {*}; "[$r]" }
	multi method render(Int $x where * > 5 --> Str) { 'big' }
	multi method render(Int $x --> Str) { 'small' }
}
role S does R { }
role S2 does R { method other() { 1 } }
class Direct does R { }
class Nested does S { }
class Nested2 does S2 { }
class Twice does S2 does R { }

is Direct.new.render(1), '[small]', 'direct composition: the proto body wraps the result';
is Nested.new.render(1), '[small]', 'composed through a role: the proto body wraps the result';
is Nested.new.render(9), '[big]', 'and the more specific candidate is chosen';
is Nested2.new.render(1), '[small]', 'a role with a method of its own in between';
is Twice.new.render(9), '[big]', 'the same ancestor reached twice';

role H {
	sub wrap(Str $s, Bool() $p --> Str) { $p ?? "($s)" !! $s }
	proto method show(Int $x --> Str) { my $r = {*}; wrap($r, $x > 0) }
	multi method show(Int $x --> Str) { 'v' }
}
role HS does H { }
class HD does H { }
class HN does HS { }
is HD.new.show(1), '(v)', 'direct: the proto body calls a helper sub of its role';
is HN.new.show(1), '(v)', 'nested: the proto body calls a helper sub of its role';

role Own does R {
	proto method render(Int $x --> Str) { my $r = {*}; "<$r>" }
}
class OwnClass does Own { }
is OwnClass.new.render(1), '<small>', 'a nearer role proto wins over the ancestor one';

class Mine does R {
	proto method render(Int $x --> Str) { my $r = {*}; "{$r}!" }
}
is Mine.new.render(1), 'small!', "a class's own proto wins over the composed one";

role P { multi method plain(Int $x --> Str) { 'plain' } }
role PS does P { }
class PC does PS { }
is PC.new.plain(1), 'plain', 'a role without a proto still composes through a role';
