use Test;

# `$attr does R` reblesses the Attribute meta-object in place, as Rakudo does,
# so every holder of the object sees the role -- above all a trait handler
# that hands its `$attr` to another trait handler (AttrX::Mooish's
# `is custom-attr` calling `&trait_mod:<is>($attr, :mooish)`), #13263.

plan 15;

role R { has $.base-name = self.name.substr(2); }
multi trait_mod:<is>(Attribute:D $attr, :$inner!) { $attr does R; }
my @seen;
multi trait_mod:<is>(Attribute:D $attr, :$outer!) {
    &trait_mod:<is>($attr, :inner);
    @seen.push: $attr.base-name;
}
class Foo { has $.val is outer; }
class Bar { has $.val is inner; }

is-deeply @seen, ['val'], 'the calling trait handler sees the role a nested handler mixed in';
is Foo.^attributes[0].base-name, 'val', 'nested trait: the role persists on the meta-object';
is Bar.^attributes[0].base-name, 'val', 'direct trait still works';

role Hi { method hi { 'hi' } }
sub mix($a) { $a does Hi }
class C { has Int $.x is rw; }
my $attr = C.^attributes[0];
mix($attr);
is $attr.hi, 'hi', 'a sub mixing into its parameter reaches the caller\'s object';
is $attr.^name, 'Attribute+{Hi}', 'reblessed into the mixin type';
ok $attr ~~ Attribute, 'still an Attribute';
ok $attr ~~ Hi, 'does the role';
is $attr.name, '$!x', 'native method: name';
is $attr.package.^name, 'C', 'native method: package';
ok $attr.rw, 'native method: rw';
is $attr.get_value(C.new(:x(5))), 5, 'native method: get_value';

# A role stub for a method a built-in parent implements is satisfied by it.
role S { method abs {...} }
class Y is Int does S { }
is Y.new(-3).abs, 3, 'a built-in parent satisfies a role stub';

# The role a mixin type composes ranks beside the object's own class.
role CX { }
class P { }
multi sub rank(CX $, $) { 'role' }
multi sub rank(P $, Mu $) { 'class' }
my $p = P.new;
$p does CX;
is rank($p, 1), 'role', 'a reblessed object ranks its mixed-in role beside its class';

# A mixin type's role method runs in the role's package: a `samewith` in a
# multi sub the method declares finds it (AttrX::Mooish's `set-option`).
module M {
    role Opts {
        method go(@l) {
            my @out;
            proto sub opt(|) {*}
            multi sub opt(Pair:D $o) { samewith($o.key, $o.value) }
            multi sub opt(Str:D $n, Mu $v) { @out.push: "$n=$v" }
            opt($_) for @l;
            @out
        }
        method pkg { $?PACKAGE.^name }
    }
}
my $q = P.new;
$q does M::Opts;
is-deeply $q.go([a => 1]), ['a=1'], 'samewith in a role method\'s sub after a run-time mixin';
is $q.pkg, 'M::Opts', 'the role method runs in the role\'s package';
