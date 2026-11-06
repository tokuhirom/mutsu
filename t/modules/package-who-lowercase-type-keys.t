use Test;

plan 6;

module M {
    our $v2 is export = 2;
    class c1 { }
    our class c4 { }
    our $v1 = 1;
}

# A declared type is a package member under its bare name whatever its case;
# only a real `our $x` is keyed `$x`.
my @keys = M.WHO.keys.sort;
ok 'c1' ∈ @keys, 'a lowercase class is keyed c1';
ok 'c4' ∈ @keys, 'a lowercase our class is keyed c4';
nok '$c1' ∈ @keys, 'no $c1 scalar key for the class';
nok '$c4' ∈ @keys, 'no $c4 scalar key for the class';
ok all('$v1', '$v2') ∈ @keys, 'real scalars keep their $';
nok 'v2' ∈ @keys, 'the exported scalar has no sigil-less duplicate';
