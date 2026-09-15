use Test;

# A module re-exports another module's routines by binding them into its own
# export stash:
#
#     my package EXPORT::DEFAULT {
#         OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
#     }
#
# That is `JSON::Class.rakumod:117-119` verbatim -- it re-exports
# `JSON::Marshal`'s attribute traits (`is json-skip-null`, `is marshalled-by`,
# ...). The stash contents ARE the module's export list, so `use`-ing the
# re-exporting module has to import them, exactly as if it had declared them
# `is export` itself. mutsu used to import nothing at all from such a module:
# the bind registered the routine under the `EXPORT::DEFAULT` package and
# nothing attributed it to the module being loaded, so an attribute trait
# reached this way died as `Can't use unknown trait 'is' -> ...` (#8121).
#
# Measured against raku v2026.07; this file passes verbatim there too.

use lib 't/lib';
use ReExportTraitRelay;

plan 6;

ok &trait_mod:<is>.defined, 'the re-exported trait_mod:<is> is visible to the importer';

class WithTrait {
    has Str $.id is re-export-mark;
}

is WithTrait.^attributes.map(*.name), ('$!id',), 'the class with the re-exported trait declares its attribute';
is WithTrait.^attributes[0].re-export-mark-value, 'marked',
    'and the re-exported trait actually ran on it';

is re-export-greet(), 'hi', 'an ordinary re-exported sub is callable too';

# The provider was never `use`d here, so only what the relay re-exported is
# visible -- a re-export is not a wholesale import of the provider.
ok !::('ReExportTraitProvider').defined, 'the provider itself is not imported';

# A second class still dispatches the trait: the import is a lasting one, not
# a one-shot consumed by the first declaration.
class WithTraitAgain {
    has Int $.n is re-export-mark;
}
is WithTraitAgain.^attributes[0].re-export-mark-value, 'marked',
    'the re-exported trait keeps dispatching for later declarations';
