use v6;
use lib 't/lib';
use Test;

# `Str.AST($slang)` parses its invocant under the localized surface syntax of
# the `L10N::<$slang>` distribution: the module's role is loaded through the
# ADR-0026 slang-activation machinery, and its `token <category>-<name>`
# declarations plus its `core2ast` `%mapping` become that sub-parse's keyword
# vocabulary. Came from the `L10N::JA` distribution, whose whole test suite is
# one `Q:to/CODE/.AST("JA").EVAL` of Japanese-spelled Raku.
#
# `L10N::Testish` (t/lib/L10N/Testish.rakumod) is a miniature of the generated
# roles those distributions ship.

plan 14;

# --- replacement categories: the localized spelling stands in for the keyword.
my $ast = 'mine $x = 41; $x + 1'.AST('Testish');
ok $ast.defined, 'a localized declarator parses to an AST';
is $ast.EVAL, 42, 'and evaluates to what the canonical spelling would';

is 'mine $x = 2; iffy $x == 2 { 10 } elsey { 20 }'.AST('Testish').EVAL, 10,
    'localized block keywords drive the conditional';

is 'klass K { }; K.^name'.AST('Testish').EVAL, 'K',
    'a localized package declarator declares a class';

is 'subby f() { 3 }; f()'.AST('Testish').EVAL, 3,
    'a localized routine declarator declares a callable sub';

# A statement prefix is recognized by the bareword-term production (by matching
# the parsed identifier against a fixed string), not through the `keyword()`
# seam the block/scope/routine categories hook -- so it needs its own
# translation site. Without it `试试 { 10 / 2 }` under `L10N::ZH` read as a bare
# word instead of as a `try` block.
is 'mine $x = tryish { 10 / 2 }; $x'.AST('Testish').EVAL, 5,
    'a localized statement prefix parses in expression position';
is 'tryish { die "boom" }; 7'.AST('Testish').EVAL, 7,
    'and in statement position, where it still swallows the exception';

# A non-ASCII spelling, matched on a character boundary rather than a byte one.
is 'mine $x = 0; なければ $x { 9 }'.AST('Testish').EVAL, 9,
    'a multi-byte localized keyword parses';

# --- the ASCII spelling of a *replaced* keyword is gone, as it is in rakudo.
dies-ok { 'my $x = 1'.AST('Testish') },
    'the canonical spelling of a replaced keyword no longer parses';

# --- alias categories stay additive: both spellings work.
is '(1, 2, 3).map({ $_ }).howmany'.AST('Testish').defined, True,
    'a core-routine alias parses where the canonical name would';
is 'yes'.AST('Testish').EVAL, True, 'an enum alias yields the enum value';
is 'True'.AST('Testish').EVAL, True, 'and the canonical enum name still works';

# --- the vocabulary is scoped to the sub-parse, not to this compilation unit.
is 'my $y = 5; $y'.AST.EVAL, 5,
    'the enclosing unit still parses stock Raku afterwards';

# --- an unknown language is a load failure, not a silent unlocalized parse.
#     (rakudo has no special case for "Raku" here either: `.AST("Raku")` looks
#     for `L10N::Raku` and dies when it is not installed.)
dies-ok { 'my $z = 6; $z'.AST('NoSuchLanguage') },
    'a slang with no L10N distribution fails loudly';
