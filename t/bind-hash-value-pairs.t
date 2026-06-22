use Test;

plan 14;

# A hash element holds its value in a container cell. Iterating the hash as
# pairs (.pairs / .head / .kv / .antipairs / .sort) must yield the inner value
# (matching a `%h<k>` read and `.values`), not the cell — otherwise `.elems` /
# `+` on the pair value see the cell as a single scalar item.
# Regression: Template::Mustache 91/92-specs harness does
# `%specs{$name} := @tests; for %specs.sort(*.key) -> $s { plan +$s.value }`.

my @arr = {a => 1}, {a => 2}, {a => 3};

# --- := bind into a hash slot ---
{
    my %h; %h{"k"} := @arr;
    is %h<k>.elems, 3, ':= bound — direct subscript read';
    is %h.values.head.elems, 3, ':= bound — .values';
    is %h.pairs.head.value.elems, 3, ':= bound — .pairs value';
    is %h.head.value.elems, 3, ':= bound — .head value';
    is %h.head.value.head<a>, 1, ':= bound — .head.value.head indexes the inner hash';
    is (+%h.sort(*.key).head.value), 3, ':= bound — .sort value numifies to elems';
}

# --- = assign into a hash slot ---
{
    my %h; %h{"k"} = @arr;
    is %h<k>.elems, 3, '= assigned — direct subscript read';
    is %h.pairs.head.value.elems, 3, '= assigned — .pairs value';
    is %h.sort(*.key).head.value.elems, 3, '= assigned — .sort value';
}

# --- .kv yields the inner value too ---
{
    my %h; %h{"k"} := @arr;
    my @seen;
    for %h.kv -> $key, $val { @seen.push: $val.elems }
    is @seen, (3,), '.kv value elems';
}

# --- plain scalar values are unaffected ---
{
    my %h = a => 10, b => 20;
    is %h.sort(*.key).map(*.value).List, (10, 20), 'scalar values sort/pairs unchanged';
    is %h.pairs.sort(*.key).head.value, 10, 'scalar pair value';
}

# --- antipairs over a hash with an array value ---
{
    my %h; %h{"k"} := @arr;
    my $ap = %h.antipairs.head;        # value (array) becomes the key
    is $ap.key.elems, 3, '.antipairs — array value-as-key keeps its elems';
    is $ap.value, 'k', '.antipairs — original key becomes value';
}
