use Test;

plan 3;

is Set(\(1, 2)).elems, 2, 'a positional Capture coerces to a Set';
is Set(\(1..3)).elems, 1, 'a Range in a Capture remains one Set element';
is Set(\(:a, :b)).elems, 0, 'named Capture arguments are not Set elements';
