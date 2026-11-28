use Test;

# A read of a `:=`-bound or captured variable goes through the slot's binding
# cell without cloning the cell's handle (#19291). These pin that the read
# still sees every write made through either name, through a closure, and
# from another thread, and that a cell holding an undefined value reads as it.

plan 9;

{
    my $seed = 7;
    my $sref := $seed;
    $seed = 8;
    is $sref, 8, 'a bound scalar reads a write made through the source';
    $sref = 9;
    is $seed, 9, 'and the source reads a write made through the alias';
}

{
    my @data = ^4;
    my @alias := @data;
    @data[1] = 'x';
    is @alias[1], 'x', 'a bound array reads an element written through the source';
    my $sum = 0;
    $sum += @alias[$_] for 2, 3;
    is $sum, 5, 'and reads in a loop';
}

{
    my $n = 1;
    my &bump = { $n++ };
    bump() for ^3;
    is $n, 4, 'a captured scalar reads the closure writes';
}

{
    my $v = 0;
    my $w := $v;
    await start { $v = 42 };
    is $w, 42, 'a bound scalar reads a write made on another thread';
}

{
    my $u;
    my $b := $u;
    nok $b.defined, 'a bound scalar holding no value reads as undefined';
    $u = 'now';
    is $b, 'now', 'and then reads the value assigned';
}

{
    my %h = a => 1;
    my %alias := %h;
    %h<b> = 2;
    is %alias<b>, 2, 'a bound hash reads a key added through the source';
}
