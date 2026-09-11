use v6;
use lib 't/lib';
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# `token`/`regex`/`rule` declarator accepts traits, and `is export` makes the
# Regex importable under `&name` exactly as `is export` on a sub does. Before
# this, `my token all-bases is export { ... }` was a hard parse error that took
# the whole compilation unit with it (Collection, Number::More).

plan 8;

# The declarator parses with a trait, with and without a scope declarator,
# with and without a tag argument.
lives-ok { EVAL 'my token t1 is export { \d+ }' }, 'my token ... is export parses';
lives-ok { EVAL 'our token t2 is export { \d+ }' }, 'our token ... is export parses';
lives-ok { EVAL 'token t3 is export(:tag) { \d+ }' }, 'token ... is export(:tag) parses';
lives-ok { EVAL 'my rule t4 is export { \d+ }' }, 'my rule ... is export parses';
lives-ok { EVAL 'my regex t5 is export { \d+ }' }, 'my regex ... is export parses';

# An untagged export is NOT imported by a plain `use` — checked before any
# tagged import, since a name stays visible once imported in this process.
{
    my $imported = True;
    try {
        $imported = EVAL q:to/CODE/;
            use ExportedRegexDeclarator;
            (&wordy ~~ Regex).so
            CODE
        CATCH { default { $imported = False } }
    }
    nok $imported, 'a tagged-only token is not imported by a plain use';
}

{
    use ExportedRegexDeclarator;
    is ~("abc42" ~~ &digits), '42', 'an exported token is importable as &name';
    # The same import must also satisfy `<name>` inside the importer's regex.
    ok "abc42" ~~ / <digits> /, 'an exported token resolves as <name> in a regex';
}
