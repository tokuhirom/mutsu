use Test;

# An angle-bracket subscript key balances nested `<...>`, so the key is the whole
# balanced content: `%h<a<b>>` has the key `a<b>` and
# `%EXPORT<&trait_mod:<is>>` the key `&trait_mod:<is>`. The subscript parser
# stopped at the first `>`, leaving a stray `>` that broke the expression
# (Trait::Env: `%EXPORT<&trait_mod:<is>> = &trait_mod:<is>;`).

plan 8;

{
    my %h;
    %h<a<b>> = 5;
    is %h.keys.sort.join(','), 'a<b>', 'a nested-angle key is a single literal key';
    is %h<a<b>>, 5, 'reading back the nested-angle key works';
}

{
    my %EXPORT;
    %EXPORT<&trait_mod:<is>> = 42;
    is %EXPORT.keys.sort.join(','), '&trait_mod:<is>',
        'the Trait::Env key `&trait_mod:<is>` is one key';
    is %EXPORT<&trait_mod:<is>>, 42, 'reading it back works';
}

# No regressions on the ordinary subscript-key forms.
{
    my %h;
    %h<foo> = 1;
    is %h<foo>, 1, 'a plain key still works';
}
{
    my %h;
    %h<=> = 2;
    is %h.keys.sort.join(','), '=', 'the `=` key still works';
}
{
    my %h = a => 1, b => 2;
    is ~%h<a b>, '1 2', 'a multi-word angle slice key still works';
}
{
    my %h;
    %h<(default)> = 9;
    is %h.keys.sort.join(','), '(default)', 'a parenthesized key still works';
}
