use Test;

# `module Foo:auth<...>:ver<...> { ... }` (block form with adverbs) is wrapped
# by the parser in a SyntheticBlock alongside its metadata statements. The
# importer's static export scan did not walk into that wrapper, so every
# `is export` symbol operator of such a module failed to parse at its use
# site (PatternMatching). An operator exported as a code variable
# (`our &infix:<┇> is export = &[other]`) must be learned by the scan too.

plan 4;

use lib 't/lib';
use AdverbedBlockModuleOps;

is 1 ⊕ 2, 3, 'symbol operator exported from an adverbed module block';
is (5 ┇ -> $a { $a + 1 }), 6, 'operator exported as `our &infix:<...> is export` variable';
is &infix:<┇>(1, * + 1), 2, 'the exported operator variable is callable by name';
is ANSWER, 42, 'exported constant from an adverbed module block';
