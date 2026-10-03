use Test;
use lib $*PROGRAM.parent(3).add('lib');

plan 4;

# `use MergeOuterBase::Kid` merges the package `MergeOuterBase` that the
# module's `role MergeOuterBase::Kid` lives in; in that module the name is
# the class it `use`d, so the bare name resolves here (Monad's
# `$x ~~ Monad` after only `use Monad::Maybe`).
{
    use MergeOuterBase::Kid;
    is MergeOuterBase.^name, 'MergeOuterBase', 'the outer package name resolves';
    is MergeOuterBase.hello, 'base', '... to the class itself';
    ok MergeOuterBase::Kid.new ~~ MergeOuterBase, 'smartmatch against it';
}

# A module that only `use`s the class without nesting under it does not
# re-export it (ADR-11136).
{
    use MergeOuterOther::Kid;
    throws-like 'MergeOuterBase.^name', X::Undeclared::Symbols,
        'an unrelated nesting does not leak the name';
}
