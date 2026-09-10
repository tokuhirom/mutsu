use Test;

plan 25;

class Foo { has $.x }
class Bar { method raku { "CUSTOM" } }

my $f = Foo.new(x => 1);

# The instance's own .raku was already right; the nested cases rendered `Foo()`
# because the pure renderer cannot dispatch a method.
is $f.raku, 'Foo.new(x => 1)', 'a bare instance';
is [$f].raku, '[Foo.new(x => 1)]', 'an instance in an Array';
is ($f,).raku, '(Foo.new(x => 1),)', 'an instance in a List';
is %(a => $f).raku, '{:a(Foo.new(x => 1))}', 'an instance as a Hash value';
is (a => $f).raku, ':a(Foo.new(x => 1))', 'an instance as a Pair value';
is ($f => 1).raku, 'Foo.new(x => 1) => 1', 'an instance as a Pair key';
is [[$f],].raku, '[[Foo.new(x => 1)],]', 'a nested Array';
is $[$f].raku, '$[Foo.new(x => 1)]', 'an itemized Array';
is [$f].Seq.raku, '(Foo.new(x => 1),).Seq', 'an instance in a Seq';
is ($f,).Set.raku, 'Set.new(Foo.new(x => 1))', 'an instance in a Set';
is ($f => 2,).Mix.raku, '(Foo.new(x => 1)=>2).Mix', 'an instance in a Mix';
is \($f, x => $f).raku, '\(Foo.new(x => 1), :x(Foo.new(x => 1)))', 'an instance in a Capture';
is {a => $f}.Map.raku, 'Map.new((:a(Foo.new(x => 1))))', 'an instance in a Map';

my Foo @typed = $f, Foo.new(x => 2);
is @typed.raku, 'Array[Foo].new(Foo.new(x => 1), Foo.new(x => 2))', 'a typed Array of instances';

# A user `method raku` wins, nested as well as bare.
is Bar.new.raku, 'CUSTOM', 'a user method raku';
is [Bar.new].raku, '[CUSTOM]', 'a user method raku, nested';

# Built-in object types have the same problem and the same fix.
is (1.Supply, 2.Supply).raku, '(Supply.new, Supply.new)', 'Supply instances in a List';
is [Date.new(2020, 1, 1)].raku, '[Date.new(2020,1,1)]', 'a Date in an Array';
is [Version.new('1.2')].raku, '[v1.2]', 'a Version in an Array';

# A collection with nothing to dispatch keeps the pure renderer's output.
is (1, 2).raku, '(1, 2)', 'a plain List is untouched';
is (Foo,).raku, '(Foo,)', 'a type object is not an instance';

# A `$`-sigil attribute is a Scalar container, so its aggregate is itemized.
is Foo.new(x => [1, 2]).raku, 'Foo.new(x => $[1, 2])', 'an aggregate in a $-attribute';

# Cyclic structures must terminate: an object reached through its own attribute
# is not dispatched again, and a container that is its own ancestor is left to
# the renderer's cycle placeholder.
class Cyc { has @.myself }
my $c = Cyc.new;
$c.myself[0] = $c;
ok $c.raku.chars, 'a self-referencing object survives .raku';

my @circ;
@circ = 42, @circ;
ok @circ.raku.chars, 'a circular array survives .raku';

my %ch;
my @cb;
%ch = :b(%ch), :c(@cb);
@cb = %ch, @cb, 42;
ok @cb.raku.chars, 'a circular array within a circular hash survives .raku';
