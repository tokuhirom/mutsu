use Test;
use MONKEY-SEE-NO-EVAL;
plan 10;

{
    my $count = 0;
    try $count++ if True;
    if True { $count += 2 }
    is $count, 3, 'a mainline try statement keeps the next conditional separate';
}

is EVAL('my $n=0; try $n++ if True; if True { $n += 2 }; $n'), 3,
    'if-modified try ends at its semicolon';
is EVAL('my $n=0; try $n++ if False; unless False { $n += 2 }; $n'), 2,
    'a skipped try does not skip the next unless statement';
is EVAL('my $n=0; try $n++ unless False; if True { $n += 2 }; $n'), 3,
    'unless-modified try ends at its semicolon';
is EVAL('my $n=0; try $n++ for 1..3; if True { $n += 2 }; $n'), 5,
    'for-modified try ends at its semicolon';
is EVAL('my $a; try $a := 42; unless True { die "bad" }; $a'), 42,
    'a binding inside try ends at its semicolon';
is EVAL('my $n=0; try die "caught" if True; if True { $n=7 }; $n'), 7,
    'a caught exception does not swallow the next conditional';
throws-like { EVAL('try 42 if True; if True { die "outside" }') }, X::AdHoc,
    'an exception from the next statement is outside the try';

my $delimiter = 0x3a9.chr;
my $source = 'my $n=0; my $value = do 42 if Q:to<' ~ $delimiter ~ ">;\nyes\n"
    ~ $delimiter ~ "\n" ~ 'if True { $n=7 }; "$value,$n"';
is EVAL($source), '42,7', 'a rewritten Unicode heredoc tail retains its semicolon';

is EVAL('my $n=0; try $n++ if True;;; if True { $n += 3 }; $n'), 4,
    'extra empty statements remain valid separators';
