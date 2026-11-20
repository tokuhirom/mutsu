use Test;

# A routine with no locals that declares a `my package` runs on the fast call
# path once warm. The package name is a lexical of the routine: the call's
# return merge must drop it with the routine's other declarations, however
# many times the routine runs.

plan 5;

sub gone($n) { !::($n).defined && ::($n) ~~ Failure }

sub with-package { my package FastP {}; ::('FastP').^name }
is with-package(), 'FastP', 'the package resolves inside the routine';
with-package() for ^4;
ok gone('FastP'), 'the package does not outlive repeated calls';

sub caller-of { with-package() ~ '/' ~ (gone('FastP') ?? 'gone' !! 'leaked') }
is caller-of(), 'FastP/gone', 'nor a call made from another routine';
caller-of() for ^3;
ok gone('FastP'), 'still gone after warm nested calls';

my class FastP2 {}
sub shadows { my package FastP2 {}; ::('FastP2').HOW.^name }
shadows() for ^3;
is FastP2.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'an outer class of the same name is back';
