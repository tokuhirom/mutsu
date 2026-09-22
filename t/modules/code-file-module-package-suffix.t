use v6;
use Test;
use lib 't/lib';
use CodeFilePackageSuffixFixture;

# Real Raku's `Code.file` on a routine declared inside a real compilation
# unit (a `use`d module) is suffixed with " (<package>)" -- the identity the
# COMPUNIT itself was `use`d/declared under, not the routine's own (possibly
# deeper) lexical package: a method on a class nested inside a module still
# reports the module's name, never the class's. A mainline script sub's
# `.file` carries no such suffix.
#
# This is not cosmetic: `Identity::Utils`'s ecosystem-wide "did this sub come
# from my own compunit" idiom is exactly
# `&code.file.ends-with("($module)")`, used by `Code::Coverable`'s custom
# `EXPORT(*@names)` to filter `UNIT::{"&$name"}` lookups down to symbols
# actually declared in its own file. Without the suffix every such check
# silently fails and the module exports nothing (ecosystem `Code::Coverage`
# 0.0.8, whose `t/01-basic.rakutest` died with "Unknown function: bytecode"
# for exactly this reason).

ok &fixture-sub.file.ends-with('(CodeFilePackageSuffixFixture)'),
        'a module sub .file carries the module\'s own package suffix';

ok FixtureClass.^lookup('fixture-method').file.ends-with('(CodeFilePackageSuffixFixture)'),
        'a method .file reports the MODULE\'s package suffix, not the class\'s';

ok FixtureGrammar.^lookup('fixture-token').file.ends-with('(CodeFilePackageSuffixFixture)'),
        'a grammar token .file also carries the module suffix';

is &fixture-sub.file, FixtureClass.^lookup('fixture-method').file,
        'a sub and a method in the same compunit report the identical .file';

sub mainline-sub { 1 }
nok &mainline-sub.file.contains('('),
        'a mainline script sub .file has no package suffix';

ok &fixture-sub.file.split(' (')[0].IO.basename eq 'CodeFilePackageSuffixFixture.rakumod',
        'the suffixed .file still names the right file before the suffix';

done-testing;
