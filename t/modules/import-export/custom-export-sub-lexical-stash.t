use Test;

# A symbol a custom `sub EXPORT` installs must be visible to MY::/LEXICAL
# pseudo-stash lookups, exactly like a tag-based `is export` symbol is
# (#8564). `MY::`/`LEXICAL::` compile to a static lexical-scope snapshot
# (`Compiler::emit_lexical_stash`) plus the `imported_env_aliases` table that
# `import_module` (the tag-based path) populates via `record_import_env_key`
# -- the custom-`sub EXPORT` install path (`install_export_symbol`) has to
# populate the same table, or the alias is only reachable by name (bareword
# call, `.defined`), never through the pseudo-stash.
plan 3;

use lib 't/lib';
use CustomExportLexicalStashFixture;

ok &exported-by-lexical-stash.defined,
    'a custom sub EXPORT symbol resolves by bareword/&-sigil lookup';
ok MY::<&exported-by-lexical-stash>:exists,
    '...and is visible to the MY:: pseudo-stash';
is exported-by-lexical-stash(), 'from custom EXPORT',
    '...and the MY:: entry is the same imported sub';
