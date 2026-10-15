use Test;

# A `my $t` declared while a sigilless `t` is visible moves bare `t` to the
# term namespace (#11994). That relocation must keep naming the sigilless
# binding itself, not a snapshot of its value (#12441).
plan 4;

my $x = 1;
my \t := $x;
{
    my $t = 5;
    $x = 3;
    is t, 3, 'a := alias read after a shadowing my $t follows the bound variable';
    is $t, 5, 'the shadowing scalar keeps its own value';
}

my $y = 1;
my \u = $y;
{
    my $u = 5;
    $y = 3;
    is u, 3, 'a plain sigilless binding of a variable also follows it';
}

my \v = 7;
{
    my $v = 5;
    is v + $v, 12, 'a constant-valued sigilless binding is unchanged';
}
