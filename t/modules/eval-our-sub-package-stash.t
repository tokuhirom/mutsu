use v6;
use Test;

plan 5;

# An `our sub` an EVAL unit declares inside a package is published to the
# package stash, which the caller shares (#13348).
EVAL q[package Q0 { our sub v { 0 } }];
is Q0::v(), 0, 'our sub of a package declared in EVAL is callable afterwards';
is EVAL(q[Q0::v()]), 0, '... and from another EVAL';

EVAL q[module Q1 { our sub w($x) { $x * 2 } }];
is Q1::w(21), 42, 'also for a module, with arguments';

EVAL q[class Q2 { our sub k { "k" } }];
is Q2::k(), "k", 'also for a class';

EVAL q[package Q3 { sub hidden { 1 } }];
throws-like { Q3::hidden() }, Exception, 'a plain (my) sub stays out of the stash';
