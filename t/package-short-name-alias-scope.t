use v6;
use lib 't/lib';
use Test;

# todo/tickets/package-short-name-alias-is-global.md (news/2026-08 once resolved):
# a class/role declared with an already-qualified name used to register its
# short name in the single flat, global `env`, so it resolved from *anywhere*
# in the process. Two things must both be true instead:
#
#  1. The short name must NOT be globally visible -- a file-scope
#     `class Cro::Hdr { }` makes `Hdr` undeclared outside package `Cro`.
#  2. A DIFFERENT package that `use`s the declaring module must still see the
#     short name bare, from its own methods -- the common NativeCall idiom
#     `unit module Foo::Native; class Handle is repr('CPointer') {}`, then
#     `unit class Foo::Driver; use Foo::Native; method f() { Handle.new }`
#     (DBDish::Pg::Native's `PGconn`, used bare from sibling class
#     `DBDish::Pg`'s own methods, is this exact shape -- an ancestor-chain-only
#     first attempt at (1) broke this and had to be reverted, PR #6010).

plan 4;

class Cro::Hdr { }
# mutsu does not raise a compile-time "Undeclared name" error the way raku
# does (a separate, unrelated gap) -- an unresolved bareword falls through to
# a plain Str. What matters here is that it does NOT resolve to Cro::Hdr.
isnt Hdr.^name, 'Cro::Hdr',
    'a package-qualified class short name is not visible outside its package';

use SiblingPkg::First;
is SiblingPkg::First.new.make.^name, 'SiblingPkg::Native::Handle',
    'a sibling package that use-imports the declaring module for the FIRST time
     resolves the exported class bare from its own method';

use SiblingPkg::Second;
is SiblingPkg::Second.new.make.^name, 'SiblingPkg::Native::Handle',
    'a second sibling package that use-imports an ALREADY-loaded module also
     resolves the exported class bare from its own method';

use lib 'modules/URI/lib';
use URI;
is URI.new("http://example.com/foo").path.^name, 'URI::Path',
    'a class declared at file scope in its own module still resolves bare
     inside another class of the SAME declaring package (URI::Path from
     URI\'s own methods) -- the counter-example that sank an earlier naive
     "gate on current_package" attempt';
