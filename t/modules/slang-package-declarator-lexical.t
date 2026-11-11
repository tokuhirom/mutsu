use lib 't/lib';
use Test;

# A package declarator a slang adds from `EXPORT` (EEL 0.3's `event`, #16888) is
# known in the block holding the `use` and its inner blocks only, as rakudo's
# lexically scoped `$*LANG`: after the block the word is an ordinary name again.
plan 6;
{
    use LexicalDeclaratorSlang;
    gizmo Inner { method m { 'inner' } }
    is Inner.m, 'inner', 'the declarator works in the block that uses the slang';
    is Inner.HOW.gizmo-tag(Inner), 'gizmo:Inner', 'with the slang\'s metaclass';
    {
        gizmo Nested { }
        is Nested.^name, 'Nested', 'and in a nested block';
    }
    {
        use LexicalDeclaratorSlang;
    }
    gizmo After { }
    is After.^name, 'After', 'a nested `use` of the same slang leaves the outer one active';
}
throws-like 'gizmo Outer { }', X::Comp, 'it is gone after the block';
class gizmo { method k { 'a class named like the keyword' } }
is gizmo.k, 'a class named like the keyword', 'the word is an ordinary name again';
