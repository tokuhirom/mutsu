use Test;
use lib 't/lib';

# A role body's `my constant` is initialized when the body is compiled, as in
# Rakudo, so the role body's exported subs read it in an importer that never
# composes the role (CSS::Units' `dimension`, #13279).
plan 5;

use RoleBodyConstant :dimension, :plain;

is dimension('a'), 1, 'exported sub reads a constant hash';
is plain(), 5, 'exported sub reads constant hash and list';

class C does RoleBodyConstant { }
is C.new.via-method, 2, 'a composing class still reads the constant';
is dimension('b'), 2, 'composition leaves the exported sub working';

role Local { my constant X = 3 * 2; method x { X } }
class F does Local { }
class G does Local { }
is F.new.x ~ G.new.x, '66', 'a constant read by methods of two composing classes';
