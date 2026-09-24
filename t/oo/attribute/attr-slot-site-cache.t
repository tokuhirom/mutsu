use Test;

# Instances built from a class's constructor plan lay their declared
# attributes out in per-class slots, and each `$!x` / `$.x` access site in a
# method remembers the slot it resolved to for the last layout it saw
# (ADR-0121 D2/D3). These pin the cases where that memory must NOT be
# replayed, or must be replayed for the right layout only.

plan 16;

# One method body, many layouts: an inherited method runs on instances of
# several subclasses whose layouts differ (the child appends its own
# attributes), so the site's cache flips between them.
class Base {
    has $.a = 'base-a';
    method get-a { $!a }
    method set-a($v) { $!a = $v }
}
class Wide is Base { has $.w1 = 1; has $.w2 = 2; }
class Other is Base { has $.o = 'o'; }
{
    my @objs = Base.new, Wide.new(a => 'wide-a'), Other.new(a => 'other-a'), Base.new(a => 'b2');
    is @objs.map(*.get-a).join(','), 'base-a,wide-a,other-a,b2',
        'one site reads the right slot across alternating layouts';
    .set-a(.get-a ~ '!') for @objs;
    is @objs.map(*.get-a).join(','), 'base-a!,wide-a!,other-a!,b2!',
        'one site writes the right slot across alternating layouts';
}

# A private attribute declared in both a parent and a child: the choice of
# key depends on the running method's owner, not on the layout alone.
class PParent {
    has $!p = 'parent';
    method pp { $!p }
}
class PChild is PParent {
    has $!p = 'child';
    method cp { $!p }
}
{
    my $c = PChild.new;
    is "{$c.pp} {$c.cp} {$c.pp} {$c.cp}", 'parent child parent child',
        'owner-dependent private attributes are never served from the wrong owner';
}

# An instance of a class with a builtin base keeps attributes outside the
# declared slots; the declared ones still read and write correctly.
class MyErr is Exception {
    has $.detail = 'd';
    method bump { $!detail = $!detail ~ '+'; $!detail }
    method message { "err: $!detail" }
}
{
    my $e = MyErr.new;
    $e.bump;
    is $e.bump, 'd++', 'a declared attribute next to a builtin base';
    is $e.message, 'err: d++', 'and read from another method';
}

# A runtime role mixin: the role method goes to the role's own cell, the
# class's method keeps reading the class slot.
role Tagged { has $.tag = 't'; method retag($t) { $!tag = $t } }
class Carrier {
    has $.c = 'c';
    method get-c { $!c }
    method set-c($v) { $!c = $v }
}
{
    my $o = Carrier.new;
    $o.get-c; $o.set-c('c1');
    $o does Tagged;
    $o.retag('t2');
    $o.set-c('c2');
    is $o.tag, 't2', 'a role attribute on a mixin';
    is $o.get-c, 'c2', 'the class attribute on the same mixin';
}

# `augment` changes the class after instances exist, which rebuilds its
# constructor plan (and so its layout): old and new instances both keep
# working through the same method sites.
use MONKEY-TYPING;
class Grow {
    has $.g = 'g';
    method get-g { $!g }
}
{
    my $old = Grow.new;
    is $old.get-g, 'g', 'before augment';
    augment class Grow { method shout { $!g.uc } }
    my $new = Grow.new(g => 'n');
    is $new.get-g, 'n', 'a new instance after augment';
    is $new.shout, 'N', 'the added method reads the attribute';
    is $old.get-g, 'g', 'an old instance still reads through the same site';
}

# Declaration order is the attribute order.
class Ordered { has $.z = 1; has $.y = 2; has $.x = 3; }
is Ordered.new.raku, 'Ordered.new(z => 1, y => 2, x => 3)', '.raku lists attributes in declaration order';
is Ordered.^attributes.map(*.name).join(' '), '$!z $!y $!x', '.^attributes in declaration order';

# `eqv` and cloning compare and copy slot-held attributes.
{
    my $a = Ordered.new(y => 5);
    my $b = $a.clone;
    ok $a eqv $b, 'a clone is eqv to its original';
    my $c = $a.clone(x => 9);
    nok $a eqv $c, 'a clone with a changed attribute is not';
}

# A `:=`-bound attribute is still written through its container.
class Bound {
    has $.v is rw = 0;
    method set($x) { $!v = $x }
}
{
    my $o = Bound.new;
    my $alias := $o.v;
    $o.set(5);
    is $alias, 5, 'a slot write goes through a bound attribute container';
}
