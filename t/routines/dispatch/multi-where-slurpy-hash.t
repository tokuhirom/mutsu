use Test;

# A `where` clause on a slurpy hash parameter (`*%v where {...}`) was never
# evaluated at all -- neither during multi-candidate selection nor during the
# eventual argument binding -- so such a candidate matched EVERY call
# regardless of its where-clause's truth value. Found via App::ShowPath's
# dependency chain: License::SPDX's own
#
#     multi method new(*%v where { not $_.keys }) {
#         self.from-json(%?RESOURCES<data/licenses.json>.slurp);
#     }
#
# is meant to run only for a bare `License::SPDX.new` (no args); every other
# call should fall through to the default `Mu`-provided constructor. Because
# the where-clause was never checked, it instead matched every constructor
# call REGARDLESS of arguments, so `License::SPDX.from-json(...)` -- whose
# generic object-construction path calls `type.new(|%args)` with actual
# field data -- kept re-triggering the zero-args-only branch, recursively
# re-parsing the class's own bundled resource file forever.

plan 10;

# The exact reduction: a `where` clause that only accepts an EMPTY slurpy
# hash must reject a call that supplies named arguments.
{
    my @log;
    multi sub f(*%v where { not $_.keys }) {
        @log.push('special');
        'special'
    }
    multi sub f(*%v) {
        @log.push('generic');
        'generic ' ~ %v.raku
    }
    is f(), 'special', 'empty slurpy hash matches the where-guarded candidate';
    is @log[0], 'special', 'the where-guarded candidate actually ran for no args';
    is f(a => 1), 'generic ' ~ {a => 1}.raku,
        'a non-empty slurpy hash falls through to the other candidate';
    is @log[1], 'generic', 'the generic candidate ran for a non-empty hash';
}

# The same shape on a class constructor (License::SPDX's actual pattern):
# a custom zero-args `new` must not swallow a call that supplies real data.
{
    class Widget {
        has $.tag;
        multi method new(*%v where { not $_.keys }) {
            self.bless(tag => 'default');
        }
    }
    is Widget.new.tag, 'default', 'zero-arg constructor candidate runs for Widget.new()';
    is Widget.new(tag => 'explicit').tag, 'explicit',
        'a call with real args uses the default Mu constructor, not the where-guarded one';
}

# A slurpy hash `where` clause referencing `%v` itself (not `$_`) is checked
# the same way.
{
    multi sub g(*%v where { %v.elems == 0 }) { 'zero' }
    multi sub g(*%v) { 'nonzero ' ~ %v.elems }
    is g(), 'zero', 'where referencing %v directly (empty case)';
    is g(x => 1, y => 2), 'nonzero 2', 'where referencing %v directly (non-empty case)';
}

# A single candidate (no fallback) with a failing slurpy-hash `where` clause
# is a genuine binding failure, not a silent match.
{
    sub only-empty(*%v where { not $_.keys }) { 'ok' }
    is only-empty(), 'ok', 'single candidate: empty hash satisfies the where clause';
    dies-ok { only-empty(a => 1) },
        'single candidate: a non-empty hash fails the where clause and dies';
}

# vim: expandtab shiftwidth=4
