use Test;

# Imported types and sigilless terms are entries of the importing scope's
# lexical pad, so `MY::` / `LEXICAL::` see them -- whether they came from
# `is export` or from a custom `sub EXPORT`. Reduced from Needle::Compile,
# whose test checks `MY::<Type>:exists` after `use Needle::Compile <Type>`.

plan 9;

use lib 't/lib';
use ImportedTypeAndTermFixture;
use ImportedTypeAndTermHookFixture;

ok MY::<TAG-VALUE>:exists, 'an is-export constant is in MY::';
is MY::<TAG-VALUE>, 5, '...with its value';
ok MY::<TagClass>:exists, 'an is-export class is in MY::';
ok MY::<HookClass>:exists, 'a class from sub EXPORT is in MY::';
ok MY::<HookRole>:exists, 'a role from sub EXPORT is in MY::';
ok MY::<HOOK-VALUE>:exists, 'a constant from sub EXPORT is in MY::';
ok MY::<HookStr>:exists, 'a mixin type object from sub EXPORT is in MY::';
ok LEXICAL::<HookClass>:exists, '...and in LEXICAL::';
nok MY::<NoSuchImport>:exists, 'a name nobody imported is not';
