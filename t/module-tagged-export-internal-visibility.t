use lib 't/lib';
use Test;
use TaggedExportUser;

# A module's own routines must be able to call its tagged-export subs
# (`sub foo is export(:tag)`) by bare name, even though a plain `use MODULE`
# (without `:tag`) does not import those subs into the caller. mutsu hides the
# un-imported tagged export by renaming `GLOBAL::foo` to `MODULE::foo`; before
# T-029 that also hid it from the module's own grammar-action methods and
# ordinary methods, which dispatched via bare-name resolution and died with
# "Unknown function". (POFile's `PO::Actions.TOP` -> `po-unquote`, and
# `POFile::Entry.Str` -> `po-quote`.)

plan 4;

# The tagged export is NOT imported into this program (no :helpers tag).
ok !MY::<&decorate>.defined, 'tagged export is not imported without its tag';

# A grammar action method of the module can call the tagged export.
is TaggedExportUser.parse('hello'), '<hello>',
    'grammar action resolves module tagged export';

# An ordinary method of the module can call the tagged export.
is TaggedExportUser::Widget.new(name => 'x').rendered, '<x>',
    'ordinary method resolves module tagged export';

# Importing WITH the tag makes the sub available to the caller too.
{
    use TaggedExportUser :helpers;
    is decorate('y'), '<y>', 'explicit :tag import brings the sub into scope';
}
