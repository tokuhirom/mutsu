use Test;

# A closure's `:=` of a captured `@`/`%` variable reaches the declaring frame
# even after the variable was passed to a routine's `$` parameter (#17454;
# EEL 0.3's fork machinery restores `:my %state` through such a closure).

plan 7;

sub copy($value) { 1 }

{
    my %state = count => 0;
    my &ws = -> $_ { %state := $_ };
    copy(%state);
    ws({count => 5});
    is %state<count>, 5, 'hash rebind after a call is seen by the frame';
}

{
    my @a = 1;
    my &rb = -> $v { @a := $v };
    copy(@a);
    rb([5, 6]);
    is @a.join(','), '5,6', 'array rebind after a call is seen by the frame';
}

{
    my $x = 1;
    my &rb = -> $v { $x := $v };
    copy($x);
    rb(5);
    is $x, 5, 'scalar rebind after a call is seen by the frame';
}

{
    my %h = a => 1;
    my &rb = -> $v { %h := $v };
    my &read = { %h<b> // 'none' };
    copy(%h);
    rb({b => 2});
    is read(), 2, 'a sibling closure sees the rebind';
}

{
    my %h = a => 1;
    my %alias := %h;
    my &rb = -> $v { %h := $v };
    copy(%h);
    rb({b => 2});
    is %h.keys.join, 'b', 'the rebound name has the new hash';
    is %alias.keys.join, 'a', 'a name bound to the old hash keeps it';
}

{
    my %h = a => 1;
    sub mutate($v) { $v<z> = 26 }
    my &rb = -> $v { %h := $v };
    mutate(%h);
    is %h<z>, 26, 'a callee writing through a $ parameter still reaches the hash';
}
