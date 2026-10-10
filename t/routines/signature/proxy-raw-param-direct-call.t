use Test;

plan 3;

class C { has Str $.t is rw; }
my $c = C.new;
my $store = "plain";
my \proxy = Proxy.new(FETCH => method { $store }, STORE => method (\value) { $store = value });
my $attr = C.^attributes[0];
$attr.set_value($c, proxy);

sub by-value($s) { $s }
is by-value($attr.get_value($c)), "plain", 'a plain parameter still FETCHes the Proxy';

sub typed-raw(Str $s is raw) { $s = "typed-raw" }
typed-raw($attr.get_value($c));
is $store, "typed-raw", 'a typed `is raw` parameter binds the Proxy, so assignment fires STORE';

sub untyped-rw($s is rw) { $s = "rw" }
untyped-rw($attr.get_value($c));
is $store, "rw", 'an `is rw` parameter binds the Proxy too';
