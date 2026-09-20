use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

# Two gaps found making Air::Plugin::Donate's suite pass (ecosystem parity).
# `Air::Functional` exports one sub per HTML tag (`h3`, `p`, `article`, ...) by
# generating them into its own export stash:
#
#     my package EXPORT::DEFAULT {
#         for @regular-tags -> $tag {
#             OUR::{'&' ~ $tag} := sub (*@inners, *%h) { do-regular-tag("$tag", @inners, |%h) }
#         }
#     }
#
# 1. `OUR::` names the CURRENT package, so such a binding is that package's own
#    symbol -- and its module's export when the package is an `EXPORT::<tag>`
#    stash. mutsu stored it under the literal key `&OUR::name`, which nothing
#    but the identical spelling could read, so `use Air::Functional` imported
#    none of the tag subs and every one of them was "Unknown function".
# 2. An imported CODE variable is a lexical of the importing compunit, whose
#    `env` entry the module load restores away. Reachable from the compunit's
#    mainline, it was "Unknown function" from a `sub`/`method` declared in that
#    same file.

use OurStashGen;
use OurStashConsumer;

plan 12;

# 1. The generated subs exist in the importing scope, one per loop iteration,
#    each holding its own captured `$tag`.
is aa('x'), '<aa>x</aa>', 'runtime-key OUR:: binding is exported and callable';
is bb('y'), '<bb>y</bb>', 'each generated sub closed over its own loop variable';
is &aa('z'), '<aa>z</aa>', 'the same binding is reachable through the &-sigil call form';

# The literal-key spelling, and a non-code symbol, through the same stash.
is cc(), 'cc-called', 'literal-key OUR:: code binding is exported';
is $dd, 'dd-value', 'literal-key OUR:: scalar binding is exported';

# 2. Reachable from routines declared in the importing compunit, not just its
#    mainline.
is from-sub(), '<aa>sub</aa>',
    'an imported code variable resolves from a sub in the importing compunit';
is from-sub-amp(), '<aa>amp</aa>',
    '... and through the &-sigil call form there too';
is Klass.new.from-method(), '<aa>method</aa>',
    '... and from a method of a class declared there';
is make-rolle().from-role-method(), '<bb>role</bb>',
    '... and from a method of a role declared there';
is the-scalar(), 'dd-value',
    '... and a non-code imported symbol behaves the same';

# `OUR::` outside an export stash names the enclosing package, so the binding is
# that package's own symbol -- reachable by its qualified name, as rakudo does.
package Pkg {
    OUR::<&hi> := sub { 'hi!' };
    my $name = 'dyn';
    OUR::{'&' ~ $name} := sub { 'dyn!' };
}
is Pkg::hi(), 'hi!', 'OUR:: in a package block binds that package symbol';
is Pkg::dyn(), 'dyn!', '... with a runtime-computed key too';
