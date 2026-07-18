use v6;
use Test;

# `self.bless(|%items)` (META6's `multi method new(*%items)` shape) must
# sigil-coerce named args exactly like the default constructor: a `%`-sigil
# attribute provided as a (list of) Pair(s) becomes a Hash, and a Pair with a
# non-simple key (`"Test::META" => ...` builds the general-key Pair variant)
# coerces the same way.

plan 8;

class ViaBless {
    has Str %.p;
    has Str @.a;
    multi method new(*%items) { self.bless(|%items) }
}

my $v = ViaBless.new(p => ("Test::META" => "lib/Test/META.rakumod",));
ok $v.p ~~ Hash, 'bless-provided %-attr from a one-Pair list is a Hash';
is $v.p<Test::META>, "lib/Test/META.rakumod", 'entry key/value are right';
my @kv;
for $v.p.kv -> $k, $val { @kv.push("$k=$val") }
is @kv.join(","), "Test::META=lib/Test/META.rakumod", '.kv iterates keys and values';

my $v2 = ViaBless.new(p => ("A" => "b", "C::D" => "d"));
is $v2.p.elems, 2, 'two pairs make two entries';
is $v2.p<C::D>, "d", 'general-key pair entry present';

my $v3 = ViaBless.new(a => ("x", "y"));
ok $v3.a ~~ Array, 'bless-provided @-attr from a list is an Array';
is $v3.a.join(","), "x,y", 'array elements preserved';

# Direct bless too, not just via a new multi.
class DirectBless {
    has Str %.p;
}
is DirectBless.bless(p => ("K" => "v",)).p<K>, "v", 'direct .bless coerces the %-attr as well';

done-testing;
