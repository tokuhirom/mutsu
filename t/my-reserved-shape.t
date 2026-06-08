use Test;

plan 5;

# `()` immediately after the variable name in a declaration is the reserved
# shape syntax, not a valid declaration.
throws-like 'my @a()', X::Syntax::Reserved, reserved => /shape/ & /array/,
    'my @a() is the reserved () shape syntax';
throws-like 'my &a()', X::Syntax::Reserved, instead => /':()'/,
    'my &a() is reserved (suggests :() for a longname)';
throws-like 'my %h()', X::Syntax::Reserved,
    'my %h() is the reserved () shape syntax';

# Ordinary and shaped declarations are unaffected.
{
    my @ok = 1, 2, 3;
    is @ok.elems, 3, 'an ordinary array declaration still works';
}
{
    my @sh[2; 2];
    ok @sh.defined, 'a shaped array declaration still works';
}
