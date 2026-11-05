use Test;

# A sub with declared named parameters refuses a named argument none of them
# answers to ("Unexpected named argument 'b' passed"), whatever the body does;
# the light named-call path used to ignore it. Expectations measured against
# rakudo 2026.09.

plan 18;

sub f(:$a) { "ok" }
throws-like { f(:b(1)) }, X::AdHoc, :message(/"Unexpected named argument 'b' passed"/),
    'an unknown :key(value)';
throws-like { f(b => 1) }, X::AdHoc, :message(/"Unexpected named argument 'b'"/), 'an unknown fat arrow';
throws-like { f(:a(1), :b(1)) }, X::AdHoc, :message(/"'b'"/), 'a known and an unknown one';
my $x = 5;
throws-like { f(:b($x)) }, X::AdHoc, :message(/"'b'"/), 'an unknown one carrying a variable';
is f(:a(1)), 'ok', 'the known name binds';
is f(), 'ok', 'no named argument at all';

sub g(:$a, :$c) { "ok" }
throws-like { g(:b(1)) }, X::AdHoc, :message(/"'b'"/), 'two named parameters, a third name';
is g(:c(2)), 'ok', 'the second parameter';

sub h($p, :$a) { "ok" }
throws-like { h(1, :b(1)) }, X::AdHoc, :message(/"'b'"/), 'a positional and a named parameter';
is h(1, :a(2)), 'ok', 'both bound';

sub i(:$color) { $color }
sub j(:color(:$colour)) { $colour }
is j(:color(1)), 1, 'an alias key';
is j(:colour(2)), 2, 'the inner name';
throws-like { j(:hue(1)) }, X::AdHoc, :message(/"'hue'"/), 'an unknown name beside aliases';

sub k(:$a!) { "ok" }
throws-like { k(:a(1), :b(1)) }, X::AdHoc, :message(/"'b'"/), 'a required named parameter';

sub l(:$a, *%rest) { %rest.keys.sort.join(',') }
is l(:a(1), :b(1), :c(2)), 'b,c', 'a slurpy hash takes the rest';

class K { method m(:$a) { "ok" } }
is K.new.m(:b(1)), 'ok', 'a method takes any named argument (implicit *%_)';

my %h = b => 1;
throws-like { f(|%h) }, X::AdHoc, :message(/"'b'"/), 'a flattened hash';
is f(|{ a => 1 }), 'ok', 'a flattened hash of known names';
