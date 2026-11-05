use Test;
# TEMPORARY probe for #16654 CI (a failing test number reports a true condition).
plan 8;
my $c = * + 1;
my $n = *.not;
my $gist = "t/routines/closure/whatevercode-gist-raku.t".IO.slurp;
ok !($c.raku eq 'WhateverCode.new'), 'p1';
ok !($c.raku eq '* + 1'), 'p2';
ok !($n.raku eq 'WhateverCode.new'), 'p3';
ok !($n.raku eq '*.not'), 'p4';
ok !($gist.contains("'WhateverCode.new'")), 'p5 old gist content';
ok !($gist.contains("'* + 1'")), 'p6 new gist content';
ok !("t/routines/closure/whatevercode-raku-source.t".IO.e), 'p7 raku-source exists';
ok !($*EXECUTABLE.Str.contains('release')), 'p8 release';
