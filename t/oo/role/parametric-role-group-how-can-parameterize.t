use Test;
use nqp;

plan 2;

# `Blob` is a parametric role group: its HOW has `parameterize`.
role R[::T] { }
is nqp::can(R.HOW, "parameterize"), 1, 'a role group HOW has parameterize';
is R.HOW.parameterize(R, Int).^name, 'R[Int]', 'and it parameterizes';
