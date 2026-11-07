use Test;

# Found through Monad 0.3.0 (Monad::Either's type cache is keyed on
# `E.WHICH ~ '|' ~ L.WHICH ~ '|' ~ R.WHICH`): a parameterized role type object
# has a stable identity per distinct argument list.

plan 8;

role R[::A = Any, ::B = Any] { }

ok R[Str, Int].WHICH eq R[Str, Int].WHICH, 'the same parameterization has one identity';
ok R[Str, Int].WHICH ne R[Int, Str].WHICH, 'argument order matters';
ok R[Str, Int].WHICH ne R[Str, Str].WHICH, 'different arguments differ';
ok R[Any, Any].WHICH ne R.WHICH, 'the parameterization differs from the generic role';
like R[Str, Int].WHICH.Str, / ^ 'R[Str,Int]|U' \d+ $ /, 'rendered as Name[Args]|U<id>';
ok R[Str, Int] === R[Str, Int], '=== holds for the same parameterization';
ok R[Str, Int] !=== R[Int, Str], '!=== for different ones';

my %cache;
for (Str, Int), (Int, Str), (Str, Int) -> ($l, $r) {
    %cache{R.WHICH ~ '|' ~ $l.WHICH ~ '|' ~ $r.WHICH}++;
}
is %cache.elems, 2, 'a WHICH-keyed cache distinguishes argument order and hits on repeats';
