use v6;
use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

plan 4;

# A `sub EXPORT` that re-exports a module's `EXPORT::ALL::` stash hands out the
# family's dispatcher with its proto, even when only the `multi` candidates
# carry `is export` (#19175; Red's `use Red` re-exports `Red::Do::EXPORT::ALL::`,
# whose unexported `proto red-do` binds the row-security context before `{*}`).
use ExportAllProto::Reexport;

is run-with({ 1 }, :label<a>), 'str:a', 'the proto body runs before a Str candidate';
is run-with({ 1 }, :with(Driver.new), :label<b>), 'driver:b',
    'the proto body runs before a typed candidate';
is run-with({ 1 }), 'str:none', 'the proto supplies its default';
is &run-with.candidates.elems, 2, 'the dispatcher still holds both candidates';
