use Test;

plan 6;

module M {
    our $v2 is export = 2;
    our @arr is export = 1, 2;
    our %h is export = a => 1;
    constant K is export = 5;
    constant $kk is export = 6;
    sub e1 is export { 1 }
    class c1 is export { }
    enum E is export <a b>;
}

# rakudo keys an exported `$` scalar by its sigil, an array/hash/code by theirs,
# and a type, enum key or sigil-less constant by its bare name.
my @keys = M::EXPORT::ALL.WHO.keys.sort;
ok '$v2' ∈ @keys, 'an exported scalar is keyed with its $';
ok '$kk' ∈ @keys, 'a `constant $kk` is keyed with its $';
nok 'v2' ∈ @keys, 'no sigil-less duplicate of the scalar';
ok all('@arr', '%h', '&e1') ∈ @keys, 'array, hash and sub keep their sigils';
ok all('K', 'c1', 'E', 'a', 'b') ∈ @keys, 'a term constant, type and enum keys are bare';
is M::EXPORT::ALL.WHO<$v2>, 2, 'the scalar entry holds the value';
