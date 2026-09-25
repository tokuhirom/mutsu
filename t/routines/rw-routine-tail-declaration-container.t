use Test;

# The tail of an `is rw` routine denotes a container even when that tail is a
# declaration or an assignment statement: `method precision is rw { state
# $p = 30 }` hands back `$p` itself, so `C.precision = 50` writes it.
# Reduced from the BigRoot distribution (its `precision` / `use-cache`
# accessors). Also pins the neighbouring plain-`=` rule: storing the result of
# an rw routine copies the value, it never aliases the routine's source.

plan 13;

class C {
    method a is rw { state $x = 30 }
    method b is rw { state Int $y = 30; }
    method c is rw { state Bool $z = True }
}
C.a = 1;
is C.a, 1, 'state declaration tail with initializer';
C.b = 2;
is C.b, 2, 'typed state declaration tail (trailing semicolon)';
C.c = False;
is C.c, False, 'Bool state accessor';

sub s is rw { state $v = 30 }
s() = 4;
is s(), 4, 'sub form';

my $outer = 0;
sub assign-tail is rw { $outer = 30 }
assign-tail() = 5;
is $outer, 5, 'an assignment tail hands back the assigned variable';

sub my-tail is rw { my $m = 30 }
my $r := my-tail();
is $r, 30, 'a my-declaration tail binds';

# A caller parameter that happens to share the state variable's name must not
# make the callee's own `state` readonly.
sub by-name($x) { C.a = $x; C.a }
is by-name(11), 11, 'caller readonly param with the same name as the state var';
sub counter { state $x = 1; $x++; $x }
sub via-param($x) { counter() }
via-param(0);
is via-param(0), 3, 'state var mutation ignores a same-named readonly caller param';

# Plain `=` from an rw routine copies.
my %h;
sub slot($k) is rw { %h{$k}{1} }
my $t = slot(2);
$t = 7;
is-deeply %h, {}, 'my $t = rw-call; $t = 7 does not write the hash';
(my $u = slot(3));
$u = 8;
is-deeply %h, {}, 'expression-form declaration copies too';
is $u, 8, '... and keeps its own value';

my $cell = 1;
sub cell is rw { $cell }
(my $w = cell());
$w = 7;
is $cell, 1, 'expression-form declaration does not alias a returned scalar';

without slot(4) { $_ = 5 }
is-deeply %h, {4 => {1 => 5}}, 'without over an rw call still binds the topic to the slot';
