use v6;
use Test;

# ADR-0056 renders NativeCall's builtin types under their real
# `NativeCall::Types::` package as a DISPLAY-ONLY qualification, keyed purely on
# the bare name. That made a user type whose name happens to collide report a
# package it was never declared in -- `class void { }; say void.^name` was
# `NativeCall::Types::void` in a script that never mentions NativeCall
# (GH #7582). The qualification now stands down for a name the program declared
# itself.
#
# `int8`/`short` are the discriminator the ticket named: they are core native
# types, not NativeCall-supplied ones, so they were already correct and must
# stay so.

plan 17;

# --- every colliding name, as a class ----------------------------------
class void      { }
class long      { }
class ulong     { }
class longlong  { }
class ulonglong { }
class size_t    { }
class ssize_t   { }
class Pointer   { }
class CArray    { }

is void.^name,      'void',      'class void reports its own name';
is long.^name,      'long',      'class long reports its own name';
is ulong.^name,     'ulong',     'class ulong reports its own name';
is longlong.^name,  'longlong',  'class longlong reports its own name';
is ulonglong.^name, 'ulonglong', 'class ulonglong reports its own name';
is size_t.^name,    'size_t',    'class size_t reports its own name';
is ssize_t.^name,   'ssize_t',   'class ssize_t reports its own name';
is Pointer.^name,   'Pointer',   'class Pointer reports its own name';
is CArray.^name,    'CArray',    'class CArray reports its own name';

# --- the core-native discriminator, unchanged --------------------------
class int8  { }
class short { }
is int8.^name,  'int8',  'a core native type name was already correct';
is short.^name, 'short', 'and stays correct';

# --- the other declarators take the same path --------------------------
role    roleshadow { }
grammar gramshadow { }
subset  subshadow of Int;
enum    enumshadow <ShadowA ShadowB>;
is roleshadow.^name, 'roleshadow', 'a role name is unaffected';
is gramshadow.^name, 'gramshadow', 'a grammar name is unaffected';

# --- `.raku` and `.gist` follow `.^name` -------------------------------
is void.raku, 'void', '.raku of the user type reports its own name';
is void.gist, '(void)', '.gist of the user type reports its own name';

# --- an instance names its own class -----------------------------------
class Holder { has $.x }
is Holder.new(x => 1).^name, 'Holder', 'an ordinary class is unaffected';

# --- a nested declaration never collided in the first place ------------
module Shadowed { our class void { } }
is Shadowed::void.^name, 'Shadowed::void', 'a nested declaration keeps its package';
