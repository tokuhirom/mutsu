use v6;
use Test;

# Code.yada is True only when the routine body is exactly a yada stub
# (`...`, `!!!`, or `???`). See Type/Routine.rakudoc.

plan 8;

ok  (sub a() { ... }).yada,     '... stub is yada';
ok  (sub b() { !!! }).yada,     '!!! stub is yada';
ok  (sub c() { ??? }).yada,     '??? stub is yada';
nok (sub d() { 1 }).yada,       'real body is not yada';
nok (sub e() { ...; 2 }).yada,  'stub plus more is not yada';
nok (sub f() { }).yada,         'empty body is not yada';

# `yada` is `Routine`'s, not `Block`'s or `Code`'s: Rakudo has no such method there.
throws-like { ({ ... }).yada }, X::Method::NotFound, 'a bare block has no yada';
throws-like { (-> { 42 }).yada }, X::Method::NotFound, 'a pointy block has no yada';

done-testing;
