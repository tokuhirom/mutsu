use v6;
use Test;

# Jupyter::Kernel's Magic::Filters does
#   $result but role { method output { $out.transform(callsame) } }
# over MockResult's `has $.output`: callsame must reach the class's own
# auto-generated accessor, not answer Nil.

plan 6;

class R { has $.output = "foo"; has $.mime = "x"; method plain { "plain" } }
my $r = R.new;

is ($r but role { method plain { "a:" ~ callsame() } }).plain, 'a:plain', 'a real method (unchanged)';
is ($r but role { method output { "b:" ~ callsame() } }).output, 'b:foo', 'the attribute accessor is the next candidate';
is ($r but role { method mime { callsame } }).mime, 'x', 'callsame as the whole body';
is ($r but role { method output { nextsame } }).output, 'foo', 'nextsame';

my $p = "<";
my $m = $r but role { method output { $p ~ callsame() ~ ">" } };
is $m.output, '<foo>', 'a closure variable around callsame';

role Q { method output { "q:" ~ callsame() } }
is ($r but Q).output, 'q:foo', 'a named role';

done-testing;
