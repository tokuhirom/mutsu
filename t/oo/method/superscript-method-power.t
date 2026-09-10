use Test;

# Superscript powers can be written in method-call syntax (`2.²`), the same as
# the adjacent form (`2²`). Both mean `2 ** 2`. Previously mutsu parsed `.²` as
# a method call and died with "No such method '²'".

plan 10;

is 2.², 4, '2.² == 2 ** 2';
is 2.³, 8, '2.³ == 2 ** 3';
is 5.², 25, '5.² == 25';
is 3.⁴, 81, '3.⁴ == 81';
is 2.⁻¹, 0.5, '2.⁻¹ == 0.5 (negative exponent)';

my $x = 3;
is $x.², 9, 'method-superscript on a variable';
is (1 + 1).², 4, 'method-superscript on a parenthesized expression';
is 2².³, 64, 'chained adjacent + method superscripts ((2**2)**3)';

# Adjacent form still works, and normal method calls are unaffected.
is 2², 4, 'adjacent superscript still works';
is 5.abs, 5, 'normal method call still works';
