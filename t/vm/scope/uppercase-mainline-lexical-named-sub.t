use Test;

# #9459: a named sub's free variable is bound to its declaration-site lexical
# through the ADR-0024 mainline capture store. That capture only accepted names
# whose first letter is lowercase (a sigil-less key cannot otherwise tell a
# scalar `$F` from a type `F`), so an uppercase-named `my` variable fell back to
# by-name lookup and a caller's shadowing `my` of the same name won: reads saw
# the caller's value and writes landed on it. The capture now accepts any
# identifier once it has established the key is a slot-backed `my` variable.

plan 10;

# The issue's shape, at mainline.
my $M = 1;
sub write-m { $M = 5 }
{ my $M = 9; write-m() }
is $M, 5, 'a mainline $M assignment from a named sub';

{
    my $F = 1;
    sub write-f { $F = 5 }
    { my $F = 9; write-f() }
    is $F, 5, 'an assignment reaches the declaration-site $F';
}

{
    my $E = 1;
    sub bind-e { $E := 5 }
    { my $E = 9; bind-e() }
    is $E, 5, 'a rebind reaches the declaration-site $E';
}

{
    my $x = 1;
    my $D := $x;
    sub bind-d { $D := 0 }
    { my $D = 9; bind-d() }
    is $D, 0, 'a rebind of a bound $D reaches the declaration site';
}

{
    my $H = 1;
    sub read-h { $H }
    my $seen;
    { my $H = 9; $seen = read-h() }
    is $seen, 1, 'a read sees the declaration-site $H, not the caller\'s';
}

{
    my @A = 1;
    sub push-a { @A.push(2) }
    { my @A = 9; push-a() }
    is-deeply @A, [1, 2], 'an uppercase array';
}

{
    my %H = a => 1;
    sub store-h { %H<b> = 2 }
    { my %H; store-h() }
    is-deeply %H.sort.List, (a => 1, b => 2), 'an uppercase hash';
}

{
    my \S = 3;
    sub read-s { S }
    my $seen;
    { my \S = 7; $seen = read-s() }
    is $seen, 3, 'a sigilless uppercase variable';
}

# A variable spelled like a type does not disturb the type.
{
    my $Int = 5;
    sub read-int { $Int }
    my $seen;
    { my $Int = 6; $seen = read-int() }
    is $seen, 5, 'a $Int variable resolves lexically';
    is Int.^name, 'Int', 'and the Int type is unaffected';
}
