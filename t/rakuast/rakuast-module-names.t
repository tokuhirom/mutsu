use v6;
use lib 't/lib';
use Test;
use experimental :rakuast;

# Names a loaded module (or an alias of a declared package) makes known at
# compile time. Rakudo 2026.09 resolves each while parsing, so the RakuAST node
# says what the name is: an enum value or a stash-bound value is a
# `Term::Name`, a package a `Type::Simple`. The conversion learns them from the
# parse-time module scan; a name it misses renders as a `Call::Name` (or is
# refused), and the round trip then looks up a routine `&NAME` instead.

plan 20;

# The `use` is part of each string, so the conversion's own parse loads the
# module and knows what it declares.
sub node(Str $source, Int $index) { $source.AST.statements[$index].expression }

# --- an enum's values qualified by the enum's name, loaded transitively ------

# `DepsShort::Dispatcher` uses `DepsShort::LC`, which declares
# `enum DepsShort::LC <New Sto>`; the enum is `our`, so its values are global.
is node('use DepsShort::Dispatcher; DepsShort::LC::New', 1).^name,
    'RakuAST::Term::Name', 'a value qualified by its enum name is a Term::Name';
is Q[use DepsShort::Dispatcher; DepsShort::LC::New].AST.EVAL.key, 'New',
    'and the round trip evaluates it';

# --- a qualified enum exported under its short name ------------------------

is node('use QualifiedEnumExport :Endian; Endian::Vax', 1).^name,
    'RakuAST::Term::Name', '`Endian::Vax` names the imported value';
is Q[use QualifiedEnumExport :Endian; Endian::Vax].AST.EVAL.Int, 1,
    'and it is the imported value, not a call';

# --- `import Module :TAG` --------------------------------------------------

my $tagged = Q[use EnumTaggedExportFixture :PlainTag; import EnumTaggedExportFixture :BareTag; C];
is node($tagged, 2).^name, 'RakuAST::Term::Name',
    'a value a later `import` selects by tag is a Term::Name';
is $tagged.AST.EVAL.key, 'C', 'and the round trip evaluates it';

# --- a module's EXPORT stashes ----------------------------------------------

is node('need NeedExportStash::Ops; NeedExportStash::Ops::EXPORT::DEFAULT', 1).^name,
    'RakuAST::Type::Simple', 'an `is export` routine makes `EXPORT::DEFAULT` a package';
is node('need NeedExportStash::Ops; NeedExportStash::Ops::EXPORT', 1).^name,
    'RakuAST::Type::Simple', 'and `EXPORT` itself';
is Q[need NeedExportStash::Ops; NeedExportStash::Ops::EXPORT::DEFAULT.WHO<&plain-export>.(2)].AST.EVAL,
    'plain(2)', 'the round-tripped stash reaches the exported routine';

# A value bound straight into a tag's stash (`EXPORT::special::<Answer> := 42`).
is node('use ManualExportStashVarMod :special; Answer', 1).^name,
    'RakuAST::Term::Name', 'a value bound into an export stash is a Term::Name';
is Q[use ManualExportStashVarMod :special; Answer].AST.EVAL, 42,
    'and the round trip evaluates it';

# --- a qualified role exported under its short name ------------------------

is node('use RakuASTModuleNames::Greeter; Greeter', 1).^name,
    'RakuAST::Type::Simple', 'an exported qualified role is known by its short name';
is Q[use RakuASTModuleNames::Greeter; Greeter].AST.EVAL.^name,
    'RakuASTModuleNames::Greeter', 'and the round trip names the role';

# --- a constant aliasing a declared package --------------------------------

my $alias = Q[
    class RakuASTAlias::Inner { enum Status <Started Finished>; class Nested { } }
    constant E = RakuASTAlias::Inner;
    E::Status::Finished; E::Nested; E
];
my @alias = $alias.AST.statements;
is @alias[2].expression.^name, 'RakuAST::Term::Name',
    'an enum value through the alias is a Term::Name';
is @alias[3].expression.^name, 'RakuAST::Type::Simple',
    'a class through the alias is a Type::Simple';
is @alias[4].expression.^name, 'RakuAST::Type::Simple',
    'and so is the alias itself';
is @alias[2].expression.name.canonicalize, 'E::Status::Finished',
    'the Term::Name keeps the spelling through the alias';
is Q[class RakuASTAlias4::Inner { }; constant E = RakuASTAlias4::Inner; E].AST.EVAL.^name,
    'RakuASTAlias4::Inner', 'the round-tripped alias is the class';
is Q[class RakuASTAlias2::Inner { enum Status <Started Finished> }; constant E = RakuASTAlias2::Inner; E::Status::Finished].AST.EVAL.value,
    1, 'the round-tripped enum value through the alias evaluates';

# --- semantics on the ordinary frontend -------------------------------------

{
    class RakuASTAlias3::Inner { enum Status <Started Finished> }
    constant A = RakuASTAlias3::Inner;
    ok A::Status::Started === RakuASTAlias3::Inner::Status::Started,
        'the alias reaches the same enum value';
}
