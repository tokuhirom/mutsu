use Test;

# IP::Addr: `has IP::Addr::Handler $.handler handles *` and `$ip.abbreviated = True`
# must assign through the delegate's rw accessor.

plan 3;

class H { has $.abbr is rw = False; has $.x = 1 }
class W { has H $.h handles *; }
my $w = W.new(h => H.new);
is $w.x, 1, 'wildcard delegation reads';
$w.abbr = True;
is $w.abbr, True, 'assignment forwarded to the delegate';
is $w.h.abbr, True, 'the delegate itself was updated';
