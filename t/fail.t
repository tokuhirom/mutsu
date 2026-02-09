use Test;
plan 2;

dies-ok { fail "oops" }, 'fail throws an error';

sub maybe-fail($x) {
    if $x < 0 { fail "negative" }
    return $x * 2;
}
is maybe-fail(5), 10, 'fail not triggered on valid input';
