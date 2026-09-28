use v6;
use lib 't/lib';
use Test;

# CORE's `trait_mod:<is>(Routine:D, :$export!, :$SYMBOL)` exports a routine
# under the stash key `:SYMBOL` names, into the current package's EXPORT
# stashes. List::AllUtils re-exports all of List::Util, List::MoreUtils and
# List::UtilsBy this way; the missing `:SYMBOL` candidate made its load die
# with "No matching candidates for proto sub: trait_mod:<is>".

plan 8;

{
    use SymbolExportRelay;
    ok !defined(::('&plain-routine')), 'an :all export is not imported by default';
    ok defined(SymbolExportRelay::<&plain-routine>), 'the stash binding is still visible';
}

{
    use SymbolExportRelay :all;
    ok defined(::('&proto-routine')), 'a re-exported proto is imported';
    is proto-routine([1, 2]), 'proto:2', 'the re-exported proto dispatches to its candidates';
    is proto-routine('x'), 'proto-str:x', 'every candidate stays reachable';
    is plain-routine([1]), 'plain:1', 'a re-exported plain routine is imported and callable';
    is renamed(), 'local', ':SYMBOL renames a local routine';
    ok !defined(::('&local-routine')), 'the routine is exported only under its :SYMBOL name';
}
