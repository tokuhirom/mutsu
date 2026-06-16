use Test;

# A plain Hash always stringifies its keys (Raku hash keys are always Str),
# even when built from bare numeric values (`my %h = 1..6`). They must report
# as Str in .keys/.pairs/.kv/.raku/.antipairs, and a numeric-looking Str pair
# key renders with quotes in .raku (`"1" => 2`), not the adverbial `:1(2)`.

plan 24;

# --- plain hash from a flat numeric list: keys are Str ---
{
    my %h = 1..6;
    is %h.keys.sort.map(*.^name).join(" "), "Str Str Str", "keys are Str";
    is %h.pairs.sort.map(*.key.^name).join(" "), "Str Str Str", "pairs keys are Str";
    is %h.kv.sort.map(*.^name).join(" "), "Str Int Str Int Str Int",
        "kv keys are Str, values Int";
    is %h.antipairs.sort.map(*.value.^name).join(" "), "Str Str Str",
        "antipairs values are Str";
    is %h.raku, '{"1" => 2, "3" => 4, "5" => 6}', "hash .raku quotes numeric keys";
    is %h.pairs.sort.map(*.raku).join(", "),
        '"1" => 2, "3" => 4, "5" => 6', "pair .raku quotes numeric keys";
}

# --- plain hash from bare values ---
{
    my %h = (1, "a", 2, "b");
    is %h.pairs.sort.map(*.key.^name).join(" "), "Str Str", "bare-value keys are Str";
    is %h{"1"}, "a", "string subscript works";
    is %h{1}, "a", "numeric subscript coerces to string key";
}

# --- Pair .raku key quoting rules (Raku <.ident>) ---
{
    is ("1" => 2).raku, '"1" => 2', "digit-leading key is quoted";
    is ("1a" => 2).raku, '"1a" => 2', "digit-leading alnum key is quoted";
    is ("1.5" => 2).raku, '"1.5" => 2', "dotted key is quoted";
    is ("a b" => 2).raku, '"a b" => 2', "space key is quoted";
    is ("-x" => 2).raku, '"-x" => 2', "leading-hyphen key is quoted";
    is ("x-" => 2).raku, '"x-" => 2', "trailing-hyphen key is quoted";
    is ("a--b" => 2).raku, '"a--b" => 2', "doubled-hyphen key is quoted";
    is ("a'" => 2).raku, '"a\'" => 2', "trailing-apostrophe key is quoted";
    is ("a1" => 2).raku, ':a1(2)', "alnum identifier uses adverbial form";
    is ("_x" => 2).raku, ':_x(2)', "underscore-leading identifier is adverbial";
    is ("a-b" => 2).raku, ':a-b(2)', "internal-hyphen identifier is adverbial";
    is ("a'b" => 2).raku, ':a\'b(2)', "internal-apostrophe identifier is adverbial";
}

# --- object hash still preserves typed keys ---
{
    my %h{Any} = (1, "a", 2, "b");
    is %h.pairs.sort.map(*.key.^name).join(" "), "Int Int",
        "object hash preserves Int keys";
    is %h{1}, "a", "object hash numeric lookup works";
}

# --- Set/Bag/Mix .hash keeps typed keys (object hash semantics) ---
{
    is set(1, 2, 3).hash.keys.sort.map(*.^name).join(" "), "Int Int Int",
        "Set.hash preserves Int keys";
}
