use v6;
use Test;

plan 6;

# A grammar method used as a subrule that delegates to a token via `self.bar`
# continues from the in-progress cursor (#11799).
grammar G {
    token TOP { <foo> }
    method foo { self.bar }
    token bar { "aaa" }
}

my $m = G.parse("aaa");
ok $m.so, 'method subrule delegating to a token matches';
is ~$m, 'aaa', 'whole match';
is ~$m<foo>, 'aaa', 'the method result is captured under its name';
is ~G.subparse("aaa"), 'aaa', 'subparse works too';

grammar P {
    token TOP { "x" <foo> "y" }
    method foo { self.bar }
    token bar { "aaa" }
}
is ~P.parse("xaaay"), 'xaaay', 'delegation at a non-zero position';
is ~P.parse("xaaay")<foo>, 'aaa', 'capture at a non-zero position';
