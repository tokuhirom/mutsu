use Test;

# A role with a named parameter puns to a class whose name spells its
# arguments (`R[Int,:cmp(...)]`); `::?CLASS:D` in its multi methods names
# that pun. The candidates still dispatch on the argument types, call
# after call (the resolution cache serves the repeats).

plan 8;

role R[::TYPE = Any, :&cmp = &infix:<cmp>] {
    has @.log;
    proto method put(|) {*}
    multi method put(::?CLASS:D: *%_) { @!log.push("named:" ~ %_.keys.sort.join(",")) }
    multi method put(::?CLASS:D: TYPE:D $x) { @!log.push("typed:" ~ $x) }
    multi method put(::?CLASS:D: Pair:D $p) { @!log.push("pair:" ~ $p.key) }
    method order($a, $b) { cmp($a, $b) }
}

class C does R[Int, :cmp(-> $a, $b { $b cmp $a })] { }

my $o = C.new;
$o.put(1) for ^3;
$o.put((a => 2));
$o.put(:b(3));
$o.put(2);
is-deeply $o.log, ["typed:1", "typed:1", "typed:1", "pair:a", "named:b", "typed:2"],
    'each argument type reaches its own candidate, repeatedly';
throws-like { $o.put("str") }, X::Multi::NoMatch, 'a Str matches no candidate of R[Int]';
is $o.order(1, 2), More, 'the role parameter :&cmp is the one the class supplied';

my @a is R;
@a.put(5) for ^2;
@a.put("x");
@a.put((k => 1));
is-deeply @a.log, ["typed:5", "typed:5", "typed:x", "pair:k"],
    'the default instantiation (TYPE = Any) takes a Str';
is @a.order(1, 2), Less, 'the default :&cmp';

class D does R[Str] { }
my $d = D.new;
$d.put("s") for ^2;
is-deeply $d.log, ["typed:s", "typed:s"], 'another instantiation has its own winners';
throws-like { $d.put(1) }, X::Multi::NoMatch, 'an Int matches no candidate of R[Str]';
throws-like { R[Int].new.put(Int) }, X::Multi::NoMatch, 'a type object misses the :D candidate';
