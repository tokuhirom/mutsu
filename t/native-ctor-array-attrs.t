use Test;

# VM-native default construction extended to untyped `@`/`%`-sigiled attributes
# (ledger §1 / phase ③). The native path handles untyped array/hash attributes
# (empty default, sigil-coerced provided values), reusing the interpreter's
# shared `coerce_attr_value_by_sigil` so the result is behavior-invariant.
# Typed-element (`Int @.nums`), `is Type`, and defaulted `@`/`%` attributes fall
# through to the interpreter. These assertions match `raku`.

plan 27;

# --- empty defaults ------------------------------------------------------
{
    class A { has @.items; has %.map; has $.name = "x" }
    my $e = A.new;
    is-deeply $e.items, [], 'empty @ attribute defaults to []';
    is-deeply $e.map, {}, 'empty % attribute defaults to {}';
    is $e.name, 'x', 'scalar default alongside @/% attrs';
    is $e.items.^name, 'Array', 'empty @ attr is an Array';
    is $e.map.^name, 'Hash', 'empty % attr is a Hash';
}

# --- provided list / array to @ ------------------------------------------
{
    class B { has @.items }
    is-deeply B.new(items => (1, 2, 3)).items, [1, 2, 3], 'list arg flattens into @';
    is-deeply B.new(items => [10, 20]).items, [10, 20], 'array arg into @';
    is-deeply B.new(items => 1..4).items, [1, 2, 3, 4], 'range arg expands into @';
    is-deeply B.new.items, [], 'no arg -> empty @';
}

# --- provided hash / pairs to % ------------------------------------------
{
    class C { has %.map }
    my $c = C.new(map => { a => 1, b => 2 });
    is $c.map<a>, 1, 'hash arg into %: key a';
    is $c.map<b>, 2, 'hash arg into %: key b';
    my $c2 = C.new(map => (x => 9, y => 8));
    is $c2.map<x>, 9, 'list-of-pairs arg into %: key x';
    is $c2.map<y>, 8, 'list-of-pairs arg into %: key y';
}

# --- mixed @ / % / typed $ -----------------------------------------------
{
    class D { has Int $.n; has @.items; has %.opts; has Str $.tag = "t" }
    my $d = D.new(n => 5, items => (1, 2), opts => { k => 1 });
    is $d.n, 5, 'mixed: typed scalar';
    is-deeply $d.items, [1, 2], 'mixed: @ attribute';
    is $d.opts<k>, 1, 'mixed: % attribute';
    is $d.tag, 't', 'mixed: scalar default';
    my $d2 = D.new(n => 7);
    is-deeply $d2.items, [], 'mixed: unprovided @ -> []';
    is-deeply $d2.opts, {}, 'mixed: unprovided % -> {}';
}

# --- inheritance with @ attributes ---------------------------------------
{
    class Base { has @.tags }
    class Derived is Base { has $.id = 0 }
    my $x = Derived.new(tags => <a b c>, id => 9);
    is-deeply $x.tags, ['a', 'b', 'c'], 'inherited @ attribute provided';
    is $x.id, 9, 'child scalar provided';
    is-deeply Derived.new.tags, [], 'inherited @ default empty';
}

# --- a `@`/`%` attr WITH a default falls through (still correct) ----------
{
    class E { has @.items = (1, 2, 3) }
    is-deeply E.new.items, [1, 2, 3], 'defaulted @ attr (interpreter path)';
    is-deeply E.new(items => (9,)).items, [9], 'defaulted @ attr, provided overrides';
}

# --- typed-element @ attr falls through (interpreter owns it) ------------
{
    class F { has Int @.nums }
    is F.new(nums => (1, 2, 3)).nums.join(","), "1,2,3", 'typed @ attr (interpreter path)';
}

# --- loop construction with @ attrs --------------------------------------
{
    class Row { has @.cells }
    my @rows = (^3).map({ Row.new(cells => ($_, $_ + 1)) });
    is-deeply @rows[0].cells, [0, 1], 'loop construction row 0';
    is-deeply @rows[2].cells, [2, 3], 'loop construction row 2';
}
