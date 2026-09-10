# Autovivifying a gap in a *nested* array element (`%h<k>[2] = v`, `@a[0][3] = v`)
# fills the skipped slots with the `Any` type object, exactly like a direct
# `@a[i] = v` — not `Nil`.
use Test;

plan 7;

{
    my %h;
    %h<k>[2] = 9;
    is-deeply %h<k>, $[Any, Any, 9], 'hash-element array autoviv fills gaps with Any';
}
{
    my %h;
    %h<a><b>[1] = 5;
    is-deeply %h<a><b>, $[Any, 5], 'deeper hash-element array autoviv fills with Any';
}
{
    my @a;
    @a[0][3] = 7;
    is-deeply @a[0], $[Any, Any, Any, 7], 'array-in-array autoviv fills with Any';
}
{
    # Matches the direct (non-nested) behavior.
    my @d;
    @d[2] = 42;
    is-deeply @d, [Any, Any, 42], 'direct array autoviv fills with Any (baseline)';
}
{
    # The filled holes are undefined type objects, not defined Nil.
    my %h;
    %h<k>[2] = 1;
    is %h<k>[0].defined, False, 'autoviv hole is undefined';
    is %h<k>[0].^name, 'Any', 'autoviv hole is the Any type object';
    is %h<k>[2], 1, 'the assigned element is intact';
}
