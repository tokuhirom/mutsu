use Test;

# A user-defined `trait_mod:<is>` on an attribute may call `.^add_method` with a
# closure that captures the trait sub's lexicals (e.g. the Attribute meta-object
# and the sigilless `\attr` parameter). Three things must hold for this to work,
# exercised together here (the Attribute::Predicate dist pattern):
#   1. The method the trait added must survive class composition (the local
#      class_def re-insert must not clobber the registry method).
#   2. A closure that escapes its creating sub must capture sigilless variables
#      (`\attr`) by value, not degrade the bare reference into a string.
#   3. An angle-bracket trait argument (`is predicate<bazzy>`) must reach the
#      trait sub as the named argument's value, not leak out as a statement.

plan 8;

multi sub trait_mod:<is>(Attribute:D \attr, :$predicate!) {
    my $name := $predicate ~~ Bool
      ?? "has-{attr.name.substr(2)}"
      !! $predicate.Str;
    my $method := method { attr.get_value(self).defined }
    $method.set_name($name);
    attr.package.^add_method($name, $method);
}

class A {
    has $.a is predicate;
    has $.b is predicate<bazzy>;
}

my $x = A.new(a => 42);
ok  $x.has-a,  'bare `is predicate` adds has-<name>, true when set';
nok $x.bazzy,  'named `is predicate<bazzy>` adds `bazzy`, false when unset';

my $y = A.new(b => 666);
nok $y.has-a,  'has-a false when a unset';
ok  $y.bazzy,  'bazzy true when b set';

# --- Isolated: escaping closure captures a sigilless binding ---
sub make-getter(\thing) { return { thing } }
is make-getter(42)(),  42, 'escaping closure captures sigilless \thing (int)';
is make-getter('hi')(), 'hi', 'escaping closure captures sigilless \thing (str)';

# --- Isolated: add_method with a closure capturing a sub-local ---
class B { }
sub install($cls, $val) { $cls.^add_method('grab', method { $val }) }
install(B, 'captured');
is B.new.grab, 'captured', '.^add_method closure keeps its captured lexical';

# --- Isolated: multi-word angle-bracket trait arg ---
my @seen;
multi sub trait_mod:<is>(Attribute:D \attr, :$tag!) { @seen = $tag.list }
class C { has $.c is tag<x y z>; }
is-deeply @seen, ['x', 'y', 'z'], 'multi-word `is tag<x y z>` passes a list';
