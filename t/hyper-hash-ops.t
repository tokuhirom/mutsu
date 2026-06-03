use Test;

plan 19;

my %a = a => 1, b => 2, c => 3;
my %b = a => 5, b => 6, c => 7;
my %c = a => 1, b => 2;
my %d = a => 5, b => 6;

# >>op<< : union of keys, missing side uses identity (0 for +)
{
    my %r = %a >>+<< %b;
    is +%r, 3, '>>+<< same keys: count';
    is %r<a>, 6, '>>+<< a';
    is %r<b>, 8, '>>+<< b';
    is %r<c>, 10, '>>+<< c';
}

{
    my %r = %c >>+<< %b;
    is +%r, 3, '>>+<< union: count';
    is %r<c>, 7, '>>+<< union: missing-left uses 0';
}

# <<op>> : intersection of keys
{
    my %r = %a <<+>> %d;
    is +%r, 2, '<<+>> intersection: count';
    is %r<a>, 6, '<<+>> a';
    is %r<b>, 8, '<<+>> b';
}

# >>op>> : left operand's keys
{
    my %r = %a >>+>> %c;
    is +%r, 3, '>>+>> left keys: count';
    is %r<a>, 2, '>>+>> a';
    is %r<c>, 3, '>>+>> missing-right uses 0';
}

# <<op<< : right operand's keys
{
    my %r = %c <<+<< %a;
    is +%r, 3, '<<+<< right keys: count';
    is %r<a>, 2, '<<+<< a';
    is %r<c>, 3, '<<+<< missing-left uses 0';
}

# Other operators
{
    my %r = %a >>*<< %b;
    is %r<a>, 5, '>>*<< a';
    is %r<b>, 12, '>>*<< b';
}

{
    my %x = a => 'foo', b => 'bar';
    my %y = a => '1', b => '2';
    my %r = %x >>~<< %y;
    is %r<a>, 'foo1', '>>~<< string concat a';
    is %r<b>, 'bar2', '>>~<< string concat b';
}
