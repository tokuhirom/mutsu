use Test;
use lib $?FILE.IO.parent.add('lib-need-nested-import').Str;

plan 1;

# `need Foo;` loads Foo without importing anything into the loading scope,
# but Foo's OWN `use` statements must still resolve normally from Foo's own
# methods -- `need` only suppresses exports flowing out to the needer, not
# imports flowing into the needed compunit itself (#7805).
need NestedImportUser;
is NestedImportUser.new.run, 'nested-import-ok',
    'a need-loaded compunit resolves a sub its own use imported, from its own method';
