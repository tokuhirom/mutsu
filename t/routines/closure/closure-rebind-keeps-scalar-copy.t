use Test;

# A `$` variable assigned from an `@`/`%` variable that a closure later
# rebinds with `:=` keeps the container it was given (#17482; EEL 0.3's
# `:my $alias = %state` in a forking pattern).

plan 8;

{
    my %state = count => 0;
    my $alias = %state;
    my &ws = -> $_ { %state := $_ };
    ws({count => 9});
    is $alias<count>, 0, 'the scalar keeps the old hash';
    is %state<count>, 9, 'the hash variable has the new one';
}

{
    my @a = 1, 2;
    my $ia = @a;
    my &wa = -> $_ { @a := $_ };
    wa([7]);
    is $ia.join(','), '1,2', 'the scalar keeps the old array';
    is @a.join(','), '7', 'the array variable has the new one';
}

{
    my %h = a => 1;
    my $s = %h;
    my &rb = -> $_ { %h := $_ };
    %h<b> = 2;
    is $s<b>, 2, 'before the rebind the scalar shares the hash';
    $s<c> = 3;
    is %h<c>, 3, 'and writes through the scalar reach the variable';
    rb({z => 26});
    %h<y> = 25;
    nok $s<y>:exists, 'after the rebind writes to the variable do not reach the scalar';
}

{
    my %state = count => 0;
    my $alias;
    my &ws = -> $_ { %state := $_ };
    my &wa = -> $_ { $alias = $_ };
    my %fresh = count => 0;
    ws(%fresh);
    wa(%fresh);
    $alias<count>++;
    is %state<count>, 1, 'a scalar and the rebound variable given one hash share it';
}
