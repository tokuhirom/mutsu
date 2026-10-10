use Test;

plan 5;

class C { has Str $.t is rw; }
my $c = C.new;
my @stored;
my \p = Proxy.new(
    FETCH => method { "f" },
    STORE => method (\v) { @stored.push(v) },
);
C.^attributes[0].set_value($c, p);

sub typed(Str $x is raw) { $x.VAR.^name }
sub typed-rw(Str $x is rw) { $x.VAR.^name }
is typed($c.t), 'Proxy', 'typed `is raw` parameter accepts the accessor Proxy';
is typed-rw($c.t), 'Proxy', 'typed `is rw` parameter accepts the accessor Proxy';

sub sigilless(\s) { s = "d3" }
my &f = &sigilless;
f(C.^attributes[0].get_value($c));
is-deeply @stored, ["d3"], 'sigilless parameter assigns through a Proxy via &-call';
sigilless(C.^attributes[0].get_value($c));
is-deeply @stored, ["d3", "d3"], 'sigilless parameter assigns through a Proxy';

sub plain(Str $x) { $x }
is plain($c.t), 'f', 'a plain typed parameter still sees the FETCHed value';
