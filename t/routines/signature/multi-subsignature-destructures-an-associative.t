use Test;

# An `Associative` argument's capture has NO positional part: rakudo's
# `{:x(1), :y(2)}.Capture.list` is `()`, and its `.hash` holds the entries. So a
# sub-signature that destructures it by name (`%h (:$x!, :$y!)`) consumes no
# positional slot at all, and its entries are checked as NAMED arguments.
#
# mutsu's multi-candidate matcher reported one positional element per hash entry
# -- the shape the positional-destructure forms read -- so an all-named
# sub-signature left every one of them unconsumed and the leftover-positional
# check rejected a candidate rakudo binds. The identical signature written as a
# plain `sub` bound fine, because a non-multi call goes straight to the binder
# and never runs the matcher.
#
# This is how `Crane::Patch` writes all six of its operation candidates
# (`multi sub patch(\c, %patch (:op($)! where {$_ eq 'add'}, :@path!, :$value!))`),
# so every `Crane.patch` call fell through to its `default` CATCH arm and came
# back as "✗ Crane accident: patch operation failed".

plan 17;

# --- the candidate matches, and dispatches on the sub-signature -------------

{
    multi sub m(%h (:$x!, :$y!)) { "both:$x$y" }
    is m({:x(1), :y(2)}), 'both:12', 'an all-named sub-signature matches a Hash';
}

{
    multi sub m2(%h (:$x!)) { 'hash' }
    multi sub m2($other)    { 'any' }
    is m2({:x(1)}), 'hash',
        'and is preferred over a plain Any candidate';
}

{
    multi sub m3($c (:$x!, :$y!)) { "scalar:$x$y" }
    is m3({:x(1), :y(2)}), 'scalar:12',
        'the `$`-sigiled spelling matches too';
}

{
    multi sub m4(%h (:$x!)) { 'map' }
    is m4(Map.new(('x', 1))), 'map', 'a Map matches the same way';
}

{
    # The shape Crane::Patch uses: a `where` on an anonymous named, selecting
    # between candidates that are otherwise identical.
    multi sub op(\c, %p (:op($)! where { $_ eq 'add' },     :@path!, :$value!)) { 'add' }
    multi sub op(\c, %p (:op($)! where { $_ eq 'remove' },  :@path!))           { 'remove' }
    multi sub op(\c, %p (:op($)! where { $_ eq 'replace' }, :@path!, :$value!)) { 'replace' }

    is op(0, { :op<add>, :path('a',), :value(1) }), 'add', 'a where picks the add candidate';
    is op(0, { :op<remove>, :path('a',) }), 'remove', 'and the remove candidate';
    is op(0, { :op<replace>, :path(()), :value(1) }), 'replace', 'and the replace candidate';
}

# --- an entry the sub-signature does not name is an unaccounted named arg ---

{
    multi sub n1(%h (:$x!)) { 'matched' }
    dies-ok { n1({:x(1), :y(2)}) },
        'an entry no parameter names is refused';
}

{
    multi sub n2(%h (:$x!, *%rest)) { 'slurped' }
    is n2({:x(1), :y(2)}), 'slurped', 'a named slurpy takes the remainder';
}

{
    # A POSITIONAL slurpy does not: there are no positional parts for it.
    multi sub n3(%h (:$x!, *@rest)) { 'matched' }
    dies-ok { n3({:x(1), :y(2)}) },
        'a positional slurpy does not account for a leftover entry';
}

{
    multi sub n4(%h (:$x!)) { 'matched' }
    dies-ok { n4({:y(2)}) }, 'a missing required named is still refused';
}

{
    multi sub n5(%h (:$x!, :$y)) { "optional:$x" }
    is n5({:x(1)}), 'optional:1', 'an unsupplied optional named is fine';
}

# --- neighbouring shapes that must not have moved --------------------------

{
    multi sub p1(@a ($first, $second)) { "pos:$first$second" }
    is p1([1, 2]), 'pos:12', 'a positional destructure of an Array still matches';
}

{
    multi sub p2($p (:key($k), :value($v))) { "pair:$k=$v" }
    is p2(2 => 'x'), 'pair:2=x', 'a Pair destructured by its named parts still matches';
}

{
    multi sub p3($p (:key($k))) { 'matched' }
    dies-ok { p3(2 => 'x') },
        q{a Pair's unnamed `value` part is still unaccounted for};
}

{
    multi sub p4(@a (:$x!)) { 'matched' }
    dies-ok { p4([1, 2]) },
        'an all-named sub-signature does not match a plain Array';
}

{
    # The non-multi spelling, which never had the bug, still binds.
    sub s1(%h (:$x!, :$y!)) { "sub:$x$y" }
    is s1({:x(1), :y(2)}), 'sub:12', 'the plain `sub` spelling is unchanged';
}
