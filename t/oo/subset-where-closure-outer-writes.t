use Test;

# #10868: a `where` block is a closure over its declaring scope, so a store
# it makes to an outer lexical is visible afterwards, and its reads see that
# scope's variables rather than whatever the checking frame holds.

plan 9;

{
    my $n = 0;
    subset C where { $n++; True };
    my C $s = 1;
    is $n, 1, 'a subset predicate write to an outer scalar survives the check';
    $s = 2;
    is $n, 2, 'and accumulates on every assignment check';
}

{
    my $m = 0;
    my $x where { $m++; True };
    $x = 5;
    is $m, 1, 'a `my $x where {...}` predicate write survives too';
}

{
    sub f { my $k = 0; subset D where { $k++; True }; my D $q = 1; $q = 2; $k }
    is f(), 2, 'inside a routine, the routine-local counter sees both checks';
}

{
    my $n = 0;
    subset E where { $n++; True };
    ok 5 ~~ E, 'a smartmatch runs the closure predicate';
    is $n, 1, 'and its write is seen';
}

{
    my $lim = 10;
    subset Small of Int where * < $lim;
    ok 5 ~~ Small, 'a curried predicate reads the declaring scope';
    $lim = 3;
    nok 5 ~~ Small, 'and sees a later write to it';
}

{
    my @log;
    subset L where { @log.push($_); True };
    my L $q = 7;
    $q = 8;
    is-deeply @log, [7, 8], 'an in-place container write still works';
}
