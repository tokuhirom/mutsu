use Test;
use lib 't/lib';
use BareMultiScope;
use CustomExportLexicalStashFixture;
use ExportWrapperShadowFixture;
use Issue8214StaticExport;
use RegexUnitLexical;

# ADR-8751: the unit's own top-level `use`s fill lexical import sets, as a
# block's `use` does. Every expectation was checked against Rakudo.

plan 16;

# A multi family the import bound reports one candidate per declaration (a
# module registers each candidate twice, and the set holds both copies).
is &bms-multi.candidates.elems, 2,
    'the &name of a top-level imported family reports each candidate once';
is &bms-multi.dispatchees.elems, 2, 'and so does .dispatchees';

# A routine a `sub EXPORT` hook handed out is found by the run-time readers.
ok MY::<&exported-by-lexical-stash>:exists,
    'MY:: lists a top-level hook routine';
ok ::('&exported-by-lexical-stash').defined,
    'a by-name lookup of a top-level hook routine finds it';

# The hook's value is the import, not a same-named routine its module `use`d
# for itself.
is export-wrapper-shadow-greet(), 'hello from wrapped',
    'a hook routine wins over a routine the hook module imported';
is &export-wrapper-shadow-greet(), 'hello from wrapped',
    'also through the &-sigil call';

# Code the unit's `use` encloses reaches the import: a method, a role's
# method, and a regex code block.
class C { method m { static-shadow() } }
is C.new.m, 'imported', 'a class method calls a top-level import';
role R { method go { static-shadow() } }
class D does R { }
is D.new.go, 'imported', 'a role method calls a top-level import';
ok 'cost GBP12' ~~ / $(money(12)) /, 'a regex code interpolation calls a top-level import';
ok 'cost GBP12' ~~ / <{ money(12) }> /, 'a regex code block calls a top-level import';

# Code compiled when it runs, not where it stands, still reaches the unit's
# imports: a phaser body, an attribute default, an enum value, a subset's
# predicate and a grammar's code block.
my @ended;
END { is @ended.join(','), 'end:imported', 'an END phaser calls a top-level import' }
END { @ended.push('end:' ~ static-shadow()) }
class Dflt { has $.v = static-shadow(); }
is Dflt.new.v, 'imported', 'an attribute default calls a top-level import';
enum Tagged (imported-tag => static-shadow().chars);
is imported-tag.value, 8, 'an enum value calls a top-level import';
subset Longer of Str where { static-shadow().chars < .chars };
ok 'a-longer-string' ~~ Longer, 'a subset predicate calls a top-level import';
nok 'short' ~~ Longer, 'and rejects by it';
grammar Gr { token TOP { (\d+) { $*seen = static-shadow() } } }
my $*seen;
Gr.parse('42');
is $*seen, 'imported', 'a grammar code block calls a top-level import';
