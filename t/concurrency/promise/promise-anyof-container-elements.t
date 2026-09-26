use Test;

plan 2;

# Regression from Uxmal: array-grep promotion leaves shared element cells, but
# Promise.anyof must still accept the promises stored in those cells.
my @promises = Promise.new, Promise.new;
is @promises.grep(*.status !~~ Kept).elems, 2, 'the promises remain planned';
isa-ok Promise.anyof(@promises), Promise,
    'anyof decontainerizes promises after array grep promotion';
