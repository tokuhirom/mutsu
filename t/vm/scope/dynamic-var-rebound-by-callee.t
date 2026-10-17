use Test;

# A routine that rebinds (`:=`) or binds by name (`nqp::bindlexdyn`) a
# dynamic variable its caller declared reaches the declaring frame without
# a return merge copying the callee's bindings back: the write is recorded
# as a runtime-name write and carried to the frame that declares the name
# (ADR-12529 §3, slice 40). A frame's own `my @*x` stays its own on each
# call. Checked against rakudo.

plan 6;

sub rebind() { $*t := $*root }
sub r() { my $*root = 'root'; my $*t = 'orig'; rebind(); $*t }
is r(), 'root', 'a callee rebinds the caller-declared dynamic';

sub inner-rebind() { my $x = 'own'; $*u := $x }
sub outer-u() { my $*u = 'u'; inner-rebind(); $*u }
is outer-u(), 'own', 'to a lexical of its own';

use nqp;
sub bind-dyn() { nqp::bindlexdyn('$*q', 8) }
sub q() { my $*q = 1; bind-dyn(); $*q }
is q(), 8, 'nqp::bindlexdyn binds the caller-declared dynamic';

sub log-of(&body) { my @*LOG; body(); @*LOG.List }
is-deeply log-of({ @*LOG.push: 'a' }), ('a',), 'a dynamic array declared per call';
is-deeply log-of({ @*LOG.push: 'b' }), ('b',), 'starts empty on the next call';

sub shadow() { my $*t = 'mine'; rebind(); $*t }
sub keeps() { my $*root = 'r'; my $*t = 'outer'; shadow(); $*t }
is keeps(), 'outer', 'a nearer declaration takes the rebind';
