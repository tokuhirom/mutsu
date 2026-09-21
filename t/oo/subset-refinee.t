use Test;

# Pod::To::Man 1.2.1 calls ^refinee on subset types.
plan 2;

subset PodToManSubset of Str;

is PodToManSubset.^refinee.^name, 'Str', 'subset refinee returns the base type';
is PodToManSubset.HOW.refinee(PodToManSubset).^name, 'Str', 'ClassHOW refinee returns the base type';
