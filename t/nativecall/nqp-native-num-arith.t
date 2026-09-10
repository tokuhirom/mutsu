use v6;
use Test;
use nqp;

# nqp::add_n/sub_n/mul_n/div_n/neg_n/abs_n were unimplemented ("Unsupported
# nqp:: op"), blocking Random::Choice (`my num @np = @p.map(-> $x {
# nqp::mul_n(nqp::div_n(Num($x),$total), $!n) });`).

plan 6;

is nqp::add_n(1.5e0, 2.25e0), 3.75e0, 'nqp::add_n';
is nqp::sub_n(5e0, 1.5e0), 3.5e0, 'nqp::sub_n';
is nqp::mul_n(2.5e0, 4e0), 10e0, 'nqp::mul_n';
is nqp::div_n(9e0, 4e0), 2.25e0, 'nqp::div_n';
is nqp::neg_n(3.5e0), -3.5e0, 'nqp::neg_n';
is nqp::abs_n(-3.5e0), 3.5e0, 'nqp::abs_n';
