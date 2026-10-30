use Test;
use lib 't/lib';

plan 4;

# BlockUseNestedOuter's own `use NativeCall` imports types and constants for
# the module itself. A block that `use`s the module owns only what the block
# imported, and nothing the module imported for itself is visible after it.
{
    use BlockUseNestedOuter;
    is outer-probe(), 'visible', 'the module works from inside the block';
    nok MY::.keys.grep(/CArray|Pointer|cglobal/), 'the module\'s own imports are not in the block\'s pad';
}
nok MY::.keys.grep(/CArray|Pointer|cglobal/), 'nor in the enclosing scope after the block';
throws-like { EVAL 'CArray.new' }, Exception, 'the module\'s own imports are not visible after the block';
