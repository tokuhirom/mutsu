use Test;

# `infix:<=:=>($a, $b)` and `&[=:=]($a, $b)` called directly are the
# container-identity operator: two distinct variables holding equal values
# are not identical, one variable is identical to itself.
plan 6;

my ($a, $b) = 3, 3;
my $c = 5;

is $a =:= $b, False, 'operator form: distinct variables';
is &infix:<=:=>($a, $b), False, '&infix:<=:=>(...) call: distinct variables';
is &[=:=]($a, $b), False, '&[=:=](...) call: distinct variables';
is &infix:<=:=>($c, $c), True, 'the same variable is identical to itself';
is &[=:=]($c, $c), True, '&[=:=] on the same variable';
my $d := $c;
is &infix:<=:=>($c, $d), True, 'a bound alias is identical';
