use Test;

# #11217: dropping a structure nested deeper than the native stack holds,
# built at run time with no parser involved, must not crash the process
# (it used to SIGSEGV in recursive Drop).

plan 3;

{
    my $a = [];
    for ^1_000_000 { $a = [$a] }
    $a = Nil;
    pass 'dropping a million nested arrays does not overflow';
}

{
    my %h = a => 1;
    for ^100_000 { %h = (x => %h.clone) }
    %h = ();
    pass 'dropping 100k nested hashes does not overflow';
}

{
    my $p = 1;
    for ^100_000 { $p = 1 => $p<> }
    $p = Nil;
    pass 'dropping 100k nested pairs does not overflow';
}
