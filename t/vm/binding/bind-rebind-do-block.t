use Test;

# A rebind whose source is a block run in place (`$t := do { ... }`): the
# stores inside the block must not consume the rebind's own bind markers, or
# the final store becomes an assignment into whatever `$t` was bound to (a type
# object: "assign requires a concrete object").

plan 6;

{
    my $t := Int;
    $t := do { my $s := Str; $s };
    is $t, Str, 'a do block that binds a variable';
}
{
    my $t := Int;
    $t := do { my $s = 3; $s };
    is $t, 3, 'a do block that assigns a variable';
}
{
    my Mu $t := Int;
    $t := do if False { 1 } else { my Mu $s := Str; $s };
    is $t, Str, 'a do if whose branch binds';
}
{
    my $t := 1;
    $t := do given 5 { my $d = $_ * 2; $d };
    is $t, 10, 'a do given';
}
{
    my Mu $target := Int;
    for <a b> -> $part {
        $target := do if $part eq 'b' { my Mu $x := Num; $x } else { Rat };
    }
    is $target, Num, 'inside a loop body, as a slang action walks a name';
}
{
    my $t := Int;
    $t := try { my $s := Str; $s };
    is $t, Str, 'a try block';
}

# vim: expandtab shiftwidth=4
