use lib 't/lib';
use Test;

# A package-less module's own exported multi/proto family must stay visible
# to its `sub EXPORT` (through `UNIT::`) on a second block-scoped `use`
# (#11246), without leaking out of either block.

plan 8;

{
    use UnitOwnMultiExport <columns>;
    is columns(), 42, 'first block-scoped use imports the family';
    is columns("a"), 's:a', '... all candidates';
}
{
    use UnitOwnMultiExport <columns:b>;
    is b(), 42, 'second block-scoped use in a sibling block renames the family';
    is b("x"), 's:x', '... all candidates';
}
{
    use UnitOwnMultiExport <matches:m>;
    is m(), 'm0', 'a proto family survives too';
    is m(7), 'm7', '... with its multis';
}
{
    use UnitOwnMultiExport <matches:n>;
    is n(3), 'm3', 'and a further block-scoped use of the proto family';
}
throws-like { EVAL 'columns()' }, X::Undeclared::Symbols,
    'the family does not leak out of the blocks';
