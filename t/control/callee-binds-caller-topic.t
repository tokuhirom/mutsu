use Test;
use nqp;

# #17453: a callee that binds its caller's `$_`, `$/` or `$!` by name
# (`nqp::bindlexdyn`, `CALLER::<$_> :=`) changes what the caller reads once the
# call returns. The callee's own topic is left alone, and so is the caller's
# when the callee only sets its own.

plan 10;

sub g($v) { nqp::bindlexdyn('$_', $v); 1 }

sub in-sub() { $_ = 0; g(5); $_ }
is in-sub(), 5, 'a sub sees the topic its callee bound';

class C { method m() { $_ = 0; g(6); $_ } }
is C.m, 6, 'so does a method';

my @seen;
for 1 { g(7); @seen.push($_) }
is-deeply @seen, [7], 'so does a for block';

sub nested-block($v) { if 1 { nqp::bindlexdyn('$_', $v) }; 1 }
sub via-nested() { $_ = 0; nested-block(9); $_ }
is via-nested(), 9, 'the bind may sit in a nested block of the callee';

my &pointy = -> $v { nqp::bindlexdyn('$_', $v) };
sub via-pointy() { $_ = 0; pointy(10); $_ }
is via-pointy(), 10, 'a pointy block binds its caller too';

sub bind-match($v) { nqp::bindlexdyn('$/', $v) }
sub reads-match() { 'a' ~~ /a/; bind-match(11); $/ }
is reads-match(), 11, '$/ is bound the same way';

sub own-topic($v) { $_ = 99; nqp::bindlexdyn('$_', $v); $_ }
sub keeps() { $_ = 0; my $own = own-topic(12); "$own $_" }
is keeps(), '99 12', "the callee's own topic is untouched";

sub caller-bind($v) { CALLER::<$_> := $v; 1 }
sub via-caller() { $_ = 0; caller-bind(8); $_ }
is via-caller(), 8, 'CALLER::<$_> := binds the caller topic';

sub caller-bind-match($v) { CALLER::<$/> := $v; 1 }
sub via-caller-match() { 'a' ~~ /a/; caller-bind-match(4); $/ }
is via-caller-match(), 4, 'CALLER::<$/> := binds the caller match';

sub only-own() { $_ = 1; 1 }
sub untouched() { $_ = 0; only-own(); $_ }
is untouched(), 0, "a callee's own topic assignment does not reach the caller";
