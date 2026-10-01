use Test;

# An exported operator declared in a bare block (or a routine body) of an
# inline module is exported at compile time, like any nested `is export`
# routine (#10543).

plan 2;

{
    module M1 { { sub infix:<nested-op>($a, $b) is export { "$a$b" } } }
    import M1;
    is &infix:<nested-op>(1, 2), '12', 'operator nested in a bare block';
}
{
    module M2 { sub f { sub prefix:<nested-pre>($a) is export { "pre$a" } } }
    import M2;
    is &prefix:<nested-pre>(1), 'pre1', 'operator nested in a routine body';
}
