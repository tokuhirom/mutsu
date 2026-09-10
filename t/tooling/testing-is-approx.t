use Test;
plan 2;

is-approx(100.0, 100.000009, "difference within tolerance");
is-approx(1.4142135, 1.41421356237, "approximate sqrt");
