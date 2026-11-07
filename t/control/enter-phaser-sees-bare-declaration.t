# A plain `my $x;` exists from the start of its block, so an ENTER phaser can assign it and the
# declaration statement, which runs later, does not reset the value (#17010).
use Test;
plan 11;

{
    my $f;
    ENTER { $f = "set-by-enter" }
    is $f, "set-by-enter", 'a bare scalar declaration keeps what ENTER stored';
}

sub s {
    my $g;
    ENTER { $g = 5 }
    $g
}
is s(), 5, 'in a routine body';

my @seen;
for 1..2 {
    my $h;
    ENTER { $h = "e$_" }
    @seen.push: $h;
}
is-deeply @seen, ["e1", "e2"], 'in a loop body, once per iteration';

{
    my $i = 3;
    ENTER { $i = 1 }
    is $i, 3, 'a declaration with an initializer still assigns afterwards';
}

{
    my @a;
    ENTER { @a.push: 7 }
    is-deeply @a, [7], 'an array declaration';
}

{
    my %h;
    ENTER { %h<k> = 1 }
    is-deeply %h, {k => 1}, 'a hash declaration';
}

my $seen-by-leave;
{
    my $k;
    LEAVE { $seen-by-leave = $k }
    ENTER { $k = 9 }
}
is $seen-by-leave, 9, 'LEAVE sees the value ENTER stored';

my $c = { my $z; ENTER { $z = 2 }; $z };
is $c(), 2, 'in a bare closure';
my $d = -> $q { my $w; ENTER { $w = 4 }; $w + $q };
is $d(1), 5, 'in a pointy block';

class K { method m { my $v; ENTER { $v = 8 }; $v } }
is K.new.m, 8, 'in a method';

{
    my Int $t;
    ENTER { $t = 6 }
    is $t, 6, 'a typed declaration';
}
