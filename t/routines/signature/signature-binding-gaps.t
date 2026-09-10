use v6;
use Test;

plan 49;

# ---------------------------------------------------------------------------
# A sigilless parameter may carry an attached sub-signature, just like a
# sigiled one: `\p(Int, Str)` binds `p` to the whole argument AND destructures
# it against the inner signature.
# ---------------------------------------------------------------------------

sub sigilless-subsig(\p (Int, Str)) { p.raku }
is sigilless-subsig((42, "answer")), '(42, "answer")',
    'sigilless param with sub-signature binds the whole argument';

sub sigilless-subsig-named(\p (Int $y, Str $s?, *%h)) {
    "{p.raku}|{$s // 'undefined'}|{%h<life>}"
}
is sigilless-subsig-named((42, life => 40, universe => 41)),
    '(42, :life(40), :universe(41))|undefined|40',
    'pairs in a destructured list bind by name, not positionally';

sub sigilless-subsig-inner(\p (Int $y, Str $s)) { "$y/$s" }
is sigilless-subsig-inner((7, "x")), '7/x',
    'sigilless sub-signature binds its inner parameters';

sub sigiled-subsig(@p (Int $y, Str $s?, *%h)) {
    "{@p.raku}|{$s // 'undefined'}|{%h.keys.sort.join(',')}"
}
is sigiled-subsig((42, life => 40, universe => 41)),
    '(42, :life(40), :universe(41))|undefined|life,universe',
    'the sigiled twin binds pairs by name too';

sub sigilless-plain(\x) { x }
is sigilless-plain(5), 5, 'a plain sigilless parameter still parses';
sub sigilless-typed(Int \x) { x }
is sigilless-typed(5), 5, 'a typed sigilless parameter still parses';
is (-> \x { x })(9), 9, 'a sigilless pointy-block parameter still parses';

class SigilessMethod { method m(\x) { x } }
is SigilessMethod.m(7), 7, 'a sigilless method parameter still parses';

my @raw-target = 1, 2;
sub sigilless-is-raw(\x) { x[0] = 99 }
sigilless-is-raw(@raw-target);
is @raw-target[0], 99, 'a sigilless parameter aliases its argument';

# ---------------------------------------------------------------------------
# Parameter.sub_signature / Parameter.modifier
# ---------------------------------------------------------------------------

my Signature $destructure = :(@array ($first, *@rest), @other);
is $destructure.params[0].sub_signature.gist, '($first, *@rest)',
    '.sub_signature answers the destructuring signature';
ok $destructure.params[1].sub_signature ~~ Signature,
    '.sub_signature of a plain parameter is a Signature';
nok $destructure.params[1].sub_signature.defined,
    '.sub_signature of a plain parameter is the type object';

my Signature $smileys = :(Str:U $a, UInt:D $b, $c);
is $smileys.params[0].modifier, ':U', '.modifier reports the :U smiley';
is $smileys.params[1].modifier, ':D', '.modifier reports the :D smiley';
is $smileys.params[2].modifier, '', '.modifier is empty without a smiley';

my Signature $alias = :(:s(:$sort));
nok $alias.params[0].sub_signature.defined,
    'a named alias chain is not reported as a sub-signature';

# ---------------------------------------------------------------------------
# The single-argument rule: `+name` (sigilless) vs `+@name` vs `*@name`.
# A sigilless `+` slurpy binds a List (or passes a Seq / lazy list through);
# a sigiled `+@` slurpy binds an Array, exactly like `*@`.
# ---------------------------------------------------------------------------

sub onearg-bare(+zape) { zape.^name }
sub onearg-array(+@zape) { @zape.^name }
sub star-array(*@zape) { @zape.^name }
sub twostar-array(**@zape) { @zape.^name }

is onearg-bare("Hey"), 'List', '+name binds a lone scalar as a List';
is onearg-bare(1, 2), 'List', '+name binds several arguments as a List';
is onearg-bare(), 'List', '+name binds no arguments as a List';
is onearg-bare((1, 2)), 'List', '+name binds a lone List as a List';
my @onearg-src = 1, 2;
is onearg-bare(@onearg-src), 'List', '+name binds a lone Array as a List';
is onearg-bare(1 ... *), 'Seq', '+name passes a lone lazy Seq through as a Seq';

is onearg-array("Hey"), 'Array', '+@name binds a lone scalar as an Array';
is onearg-array(1, 2), 'Array', '+@name binds several arguments as an Array';
is onearg-array(), 'Array', '+@name binds no arguments as an Array';
is onearg-array(@onearg-src), 'Array', '+@name binds a lone Array as an Array';
is onearg-array(1 ... *), 'List', '+@name exposes a lone lazy Seq as a List';

is star-array("Hey"), 'Array', '*@name binds a lone scalar as an Array';
is star-array(1 ... *), 'Array', '*@name binds a lone lazy Seq as an Array';
is twostar-array(1, 2), 'Array', '**@name binds an Array';

sub onearg-bare-elems(+zape) { zape.elems }
is onearg-bare-elems("Hey"), 1, '+name wraps a lone scalar in a 1-element list';
is onearg-bare-elems(@onearg-src), 2, '+name uses a lone Array as the whole list';
sub twostar-elems(**@zape) { @zape.elems }
is twostar-elems(@onearg-src), 1, '**@name never applies the single-argument rule';

# Element identity: only the sigilless `+l` and an explicit `is raw` alias the
# caller's element containers. A sigiled slurpy always rebinds.
{
    my @types is List = Mu, Any;
    is (-> *@l { @l })(@types)[0] =:= @types[0], False,
        '*@l elements are not container-identical to the source';
    is (-> +@l { @l })(@types)[0] =:= @types[0], False,
        '+@l elements are not container-identical to the source';
    is (-> +l { l })(@types)[0] =:= @types[0], True,
        '+l preserves element identity';
    is (-> *@l is raw { @l })(@types)[0] =:= @types[0], True,
        '*@l is raw preserves element identity';
}

# ...and the answer must not depend on which of them ran first.
{
    my @types is List = Mu, Any;
    is (-> +@l { @l })(@types)[0] =:= @types[0], False,
        '+@l identity is independent of evaluation order (1)';
    is (-> *@l { @l })(@types)[0] =:= @types[0], False,
        '+@l identity is independent of evaluation order (2)';
}

# ---------------------------------------------------------------------------
# `unless` / `until` accept a pointy-block parameter, binding the condition's
# OWN value (not its negation).
# ---------------------------------------------------------------------------

{
    my $seen;
    $_ = 1;
    unless 0 -> $_ { $seen = $_ }
    is $seen, 0, 'unless binds the condition value to $_, shadowing the topic';
}

{
    my $seen = 'untouched';
    unless 0 -> $x { $seen = $x }
    is $seen, 0, 'unless binds the condition value to a named parameter';
}

{
    my $seen = 'untouched';
    unless 5 -> $x { $seen = $x }
    is $seen, 'untouched', 'a true condition skips the unless block entirely';
}

{
    my $ran = 0;
    unless 0 -> { $ran = 1 }
    is $ran, 1, 'a zero-parameter pointy block still runs';
}

{
    my $ran = 0;
    unless 0 { $ran = 1 }
    is $ran, 1, 'plain unless is unaffected';
    unless 1 { $ran = 2 }
    is $ran, 1, 'plain unless with a true condition is unaffected';
}

{
    my @seen;
    my $i = 3;
    until $i == 0 -> $x { @seen.push($x); $i-- }
    is @seen.join(','), 'False,False,False',
        'until binds the condition value (which is False while looping)';
}

{
    my @seen;
    my $i = 3;
    while $i-- -> $x { @seen.push($x); last }
    is @seen.join(','), '3', 'while still binds the condition value';
}

is (if 5 -> $x { $x }), 5, 'if still binds the condition value';
is (with 7 -> $x { $x }), 7, 'with still binds the condition value';

done-testing;
