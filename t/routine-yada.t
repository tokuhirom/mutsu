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

# Blocks/pointy blocks are Code too.
ok  ({ ... }).yada,             'bare block stub is yada';
nok ({ 42 }).yada,              'bare block with body is not yada';

done-testing;
