use v6;
use Test;

plan 30;

# Object hashes key by the .WHICH identity of the key OBJECT, not by its
# stringification (PLAN 8.10). Covers the numerics/hashmap/traps doc-diff
# findings: key objects survive bulk assignment, allomorph lookups miss
# Int/Str keys, and `:{ ... }` builds a Mu-keyed object hash.

# --- declared object hash, bulk assignment ---
{
    my %h{Any} = 42 => "foo";
    is-deeply %h.keys.map(*.^name).list, ("Int",), 'bulk-assigned key keeps its Int type';
    ok %h{42}:exists, 'Int lookup hits the Int key';
    nok %h<42>:exists, 'IntStr allomorph lookup misses the Int key';
    is %h{42}, "foo", 'Int lookup returns the value';
}

# --- separate declaration and assignment ---
{
    my %h{Any};
    %h = 42 => "foo";
    is-deeply %h.keys.map(*.^name).list, ("Int",), 're-assigned key keeps its Int type';
    nok %h<42>:exists, 'allomorph lookup misses after re-assignment';
    %h{5} = "z";
    ok %h{5}:exists, 'element assignment stores an Int key';
    is-deeply %h.keys.map(*.^name).sort.list, ("Int", "Int"), 'both keys are Int';
}

# --- :{ ... } literal is a Mu-keyed object hash ---
{
    is :{ 0 => 42 }<0>.^name, 'Any', 'Int key, IntStr lookup misses (Any)';
    is :{ "0" => 42 }<0>.^name, 'Any', 'Str key, IntStr lookup misses (Any)';
    is :{ <0> => 42 }<0>, 42, 'IntStr key, IntStr lookup hits';
    is :{ 0 => 42 }{0}, 42, 'Int key, Int lookup hits';
    is :{ "0" => 42 }{"0"}, 42, 'Str key, Str lookup hits';
    my %h := :{ 0 => 42 };
    is-deeply %h.keys.map(*.^name).list, ("Int",), ':{ } literal preserves the Int key object';
}

# --- enum keys (traps.rakudoc [8]) ---
{
    enum Dog <Fido Rex>;
    my %h := :{ Dog => 42 };
    is %h{Dog}.^name, 'Any', 'Dog type-object key misses the autoquoted "Dog" Str key';
    is %h{"Dog"}, 42, 'Str lookup hits the autoquoted key';
    is %h{Fido}.^name, 'Any', 'enum value key misses too';
    my %e{Any} = Fido => 1;
    # `Fido => 1` autoquotes: the key is the Str "Fido", not the enum value
    is-deeply %e.keys.map(*.^name).list, ("Str",), 'fat-arrow autoquotes even for an enum name';
    ok %e<Fido>:exists, 'Str lookup finds the autoquoted key';
}

# --- flattening back into hashes ---
{
    my %o{Any} = 42 => "a";
    my %p = %o;
    is-deeply %p.keys.list, ("42",), 'object hash assigned to a plain hash stringifies keys';
    my %q{Any} = %o;
    is-deeply %q.keys.map(*.^name).list, ("Int",), 'object hash assigned to an object hash keeps key objects';
    my %m = %o, x => 1;
    is-deeply %m.keys.sort.list, ("42", "x"), 'flattening into a plain-hash list stringifies keys';
}

# --- mixed-type keys with the same stringification coexist ---
{
    my %h{Any};
    %h{1} = "int";
    %h{"1"} = "str";
    is %h.elems, 2, 'Int 1 and Str "1" are distinct keys';
    is %h{1}, "int", 'Int key reads back';
    is %h{"1"}, "str", 'Str key reads back';
    %h{1}:delete;
    is %h.elems, 1, ':delete removes only the Int key';
    is %h{"1"}, "str", 'Str key survives the Int delete';
}

# --- misc reads ---
{
    my %h{Any} = 1 => "a", "x" => "b";
    is-deeply %h.kv.sort.list, (1, "a", "b", "x"), '.kv yields typed keys';
    is-deeply %h{1, "x"}.list, ("a", "b"), 'mixed-type slice';
    ok %h.raku.contains('1 => "a"'), '.raku shows the Int key unquoted';
}

done-testing;
