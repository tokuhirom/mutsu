use Test;

plan 7;

module M {
    class M::c2 { }
    class M::c3 is export { }
    class c1 is export { }
}

# A declaration qualified with the enclosing package's own name is a plain
# member of that package.
my @keys = M.WHO.keys.sort;
ok all('c1', 'c2', 'c3') ∈ @keys, 'M::c2 / M::c3 are the members c2 / c3';
nok 'M' ∈ @keys, 'no self-package key';
nok 'M::c3' ∈ @keys, 'no qualified key for the export';
nok '$c2' ∈ @keys, 'no scalar key for the class';

my @export = M::EXPORT::ALL.WHO.keys.sort;
ok all('c1', 'c3') ∈ @export, 'the exports are bare names';
nok 'M::c3' ∈ @export, 'no qualified export key';
is M::c2.^name, 'M::M::c2', 'the type keeps its declared name';
