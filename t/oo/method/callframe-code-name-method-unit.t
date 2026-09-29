# From Log::Async t/14-frame.rakutest: callframe(N).code.name for a frame that
# is a method or the unit mainline.
use Test;
plan 3;

my @got;
sub show { @got.push: callframe(1).code.name }
sub foo { show }
class Foo { method bar { show } }

foo();
Foo.bar;
show;

is @got[0], 'foo', 'sub frame';
is @got[1], 'bar', 'method frame';
is @got[2], '<unit>', 'mainline frame';
