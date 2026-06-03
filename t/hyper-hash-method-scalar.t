use Test;

plan 14;

# Hyper method on a hash applies to each value, preserving keys
{
    my %h = a => 'x', b => 'y';
    my %r = %h>>.uc;
    is %r<a>, 'X', '>>.uc on hash value a';
    is %r<b>, 'Y', '>>.uc on hash value b';
    is +%r, 2, '>>.uc keeps key count';
}

# Hyper custom postfix on a hash
{
    sub postfix:<!>($n) { [*] 1..$n }
    my %a = a => 1, b => 2, c => 3;
    my %r = %a>>!;
    is %r<a>, 1, '>>! a';
    is %r<b>, 2, '>>! b';
    is %r<c>, 6, '>>! c';
    is +%r, 3, '>>! key count';
}

# Hyper op between a hash and a scalar (scalar broadcast over keys)
{
    my %a = a => 1, b => 2, c => 3;
    my %r = %a >>*>> 4;
    is %r<a>, 4, 'hash >>*>> scalar a';
    is %r<b>, 8, 'hash >>*>> scalar b';
    is %r<c>, 12, 'hash >>*>> scalar c';
}

{
    my %a = a => 1, b => 2, c => 3;
    my %r = 2 <<**<< %a;
    is %r<a>, 2, 'scalar <<**<< hash a';
    is %r<b>, 4, 'scalar <<**<< hash b';
    is %r<c>, 8, 'scalar <<**<< hash c';
    is +%r, 3, 'scalar-hash key count';
}
