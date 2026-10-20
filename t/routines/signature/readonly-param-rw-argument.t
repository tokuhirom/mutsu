use Test;

# A readonly `$` parameter is a value, not a writable container: passing it
# to an `is rw` parameter dies with X::Parameter::RW instead of writing into
# the caller's frame (#13782). Expected values are the `raku` output.

plan 9;

sub g($v is rw) { $v = 'x' }

sub f($p) { g($p); $p }
throws-like { f(1) }, X::Parameter::RW, 'a readonly parameter is not an rw argument';

sub f-copy($p is copy) { g($p); $p }
is f-copy(1), 'x', 'an `is copy` parameter is writable';

sub f-rw($p is rw) { g($p); $p }
my $z = 1;
is f-rw($z), 'x', 'an `is rw` parameter forwards its container';
is $z, 'x', '... and the write reaches the caller';

sub f-local($p) { my $q = $p; g($q); $q }
is f-local(1), 'x', 'a local copy of a parameter is writable';

sub f-loop($p) { for 1 -> $p is copy { g($p); return $p } }
is f-loop(1), 'x', 'a loop parameter shadowing the routine parameter is writable';

sub h(:$v! is rw) { $v = 'n' }
sub f-named($p) { h(v => $p); $p }
throws-like { f-named(1) }, X::Parameter::RW, 'a readonly parameter is not an rw named argument';

sub r($x is raw) { $x = 5 }
sub f-raw($p) { r($p) }
dies-ok { f-raw(1) }, 'an `is raw` parameter bound to a readonly parameter cannot be assigned';

multi m(Str $got) { m($got.chars) }
multi m(Mu $got) { g($got) }
throws-like { m('ab') }, X::Parameter::RW, 'a multi forwarding its readonly parameter';
