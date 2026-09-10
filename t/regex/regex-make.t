use Test;
plan 2;

make("alpha");
is made(), "alpha", 'made returns last value';

make(42);
is made(), 42, 'made updates';
