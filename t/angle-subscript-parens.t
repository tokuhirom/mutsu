use Test;

# Parentheses are ordinary characters inside angle-word quoting, so an angle
# *subscript* key may contain them: `%h<(default)>` is the string key
# `(default)`. mutsu's `<...>` word-list literal already accepted parens, but the
# subscript path rejected them. Found via the real-distribution compat sweep
# (App::Rak, docs/dist-compat-sweep.md), which does `%hash<(default)>`.

plan 6;

{
    my %h = '(default)' => 42, 'x' => 1;
    is %h<(default)>, 42, 'angle subscript key containing parens';
}
{
    my %h = '(a)' => 1, '(b)' => 2;
    is-deeply %h<(a) (b)>, (1, 2), 'multi-word angle subscript with parens';
}
{
    my %h = '(k)' => 0;
    if %h<(default)> -> $d { flunk 'should not enter' } else { pass 'missing paren key is falsy' }
}
{
    # the angle-bind chain (from an earlier fix) still works with a paren key
    my %h;
    my $x := %h<(k)> := 7;
    is %h<(k)>, 7, 'bind through a paren angle subscript';
}
{
    # `.<key>` dotted form
    my %h = '(z)' => 9;
    is %h.<(z)>, 9, 'dotted angle subscript with parens';
}
{
    # a real chained comparison (no subscript) is unaffected
    ok !(1 < 2 > 3), 'chained `<`/`>` comparison still parses as comparison';
}
