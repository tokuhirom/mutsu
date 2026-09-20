use Test;

# A module that re-exports an already-imported routine by binding it into its
# own `OUR::` pseudo-package --
#
#     my package EXPORT::DEFAULT {
#         OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
#     }
#
# -- made mutsu treat that name as an existing plain (single) routine in the
# *importing* scope. If the importer then declared its own `multi sub` of
# that same name inside a nested scope (a class body is the case that
# actually broke: `META6`'s `multi sub trait_mod:<is>` candidates, reached
# via `Date::Calendar::Hijri`'s dependency closure), mutsu rejected it with a
# false "Redeclaration of routine 'trait_mod:<is>'" -- even though the
# declaration *is* a `multi sub` adding a new candidate. rakudo accepts this
# without complaint (#8870).
#
# The import records the alias under the FILE's package (where `use` ran),
# while a class body's own `sub`/`multi sub` registers under the class's own
# package -- and the `&name` env binding the import installs is not
# package-scoped at all, so the exemption has to walk the same
# enclosing-package chain a bare name actually resolves through, not just the
# literal declaration-site package.
#
# Measured against raku v2026.07; this file passes verbatim there too.

use lib 't/lib';
use ReExportTraitRelay;

plan 2;

class ReExportLocalMultiConsumer {
    multi sub trait_mod:<is>(Attribute $attr, :$re-export-local-mark!) {
        $attr does role { method re_export_local_mark_value { 'local' } }
    }

    has Str $.id is re-export-mark;
    has Str $.tag is re-export-local-mark;
}

is ReExportLocalMultiConsumer.^attributes[0].re-export-mark-value, 'marked',
    'the re-exported trait_mod candidate still dispatches';
is ReExportLocalMultiConsumer.^attributes[1].re_export_local_mark_value, 'local',
    'a local multi sub trait_mod candidate declared alongside it is not a false redeclaration';
