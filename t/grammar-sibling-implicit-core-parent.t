use v6;
use Test;
use lib 't/lib';
use GrammarSiblingCore;

# A grammar declared with no explicit `is` parent always inherits the *core*
# `Grammar` Cursor, even inside a `unit module` that also declares its own
# `grammar Grammar`. A sibling grammar in the same module must NOT pick up that
# user grammar as its implicit parent (which would drag in its tokens, its
# Actions class, and its `parse` override). This mirrors the YAMLish battery,
# whose `grammar Schema::JSON {}` sits next to a 780-line `grammar Grammar` and
# must reduce its own `{ make ... }` blocks, not the main grammar's.
#
# Before the fix, `qualify_sibling_parent_name` rewrote the implicit `Grammar`
# parent of `Schema` to `GrammarSiblingCore::Grammar`, so `Schema.parse("42").ast`
# came back `("MAIN-GRAMMAR-WRAPPED",)` (routed through the main grammar's
# list-making `TOP`) instead of the `42` its own action produces.

plan 5;

is schema-parse("42"), 42,
    'sibling grammar reduces its own action, not the module Grammar';

my @mro = schema-mro();
ok @mro.grep({ $_ eq 'Grammar' }),
    'Schema inherits Grammar';
ok @mro.grep({ $_ eq 'Match' }),
    'Schema threads through core Grammar -> Match';
ok @mro.grep({ $_ eq 'Cool' }),
    'Schema threads through core Grammar -> Cool';

# The module-local `grammar Grammar` is still usable directly (blocker #2 shadow
# is intact): a reference inside the module resolves to the module grammar, which
# wraps its result in a 1-list via its own TOP action.
is-deeply main-parse("42"), ('MAIN-GRAMMAR-WRAPPED',),
    'the module-local grammar Grammar still runs its own action';
