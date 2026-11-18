use lib 't/lib';
use Test;

# A block-scoped `use` of a `sub EXPORT` module whose arguments are literal
# loads the module at parse time and imports in position. The multi family
# the hook hands out is the block's alone: it does not leak out of the block,
# into the rest of the unit or an `EVAL` (#18100).

plan 5;

{
    use UnitOwnMultiExport 'columns';
    is columns(), 42, 'the block sees the imported multi family';
    is columns('a'), 's:a', '... with all its candidates';
}
throws-like { EVAL 'columns()' }, X::Undeclared::Symbols,
    'the family does not leak out of the block';
{
    use UnitOwnMultiExport <columns>;
    is columns(), 42, 'a sibling block with a words argument imports it again';
}
throws-like { EVAL 'columns()' }, X::Undeclared::Symbols,
    '... and it does not leak out of that block either';
