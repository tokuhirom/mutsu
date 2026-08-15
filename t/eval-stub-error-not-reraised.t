# An EVAL'd string that stubs a package and never defines it raises
# X::Package::Stubbed once, from the EVAL itself. Catching that error must
# not leave the stub sitting in the global registry to be re-raised,
# uncaught, by a later check (the top-level end-of-program check, or a
# subsequent EVAL) — see todo/tickets/vendor-real-test-module.md.
use Test;
plan 2;

throws-like
    q[role Bottle[::T] { method Str { "a bottle of {T}" } }; class Wine { ... }; say Bottle[Wine].new;],
    X::Package::Stubbed,
    'stubbed-but-never-defined package raises X::Package::Stubbed once';

pass 'reached the end of the program without a second, uncaught X::Package::Stubbed';
