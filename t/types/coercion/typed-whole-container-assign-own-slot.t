use Test;

# A whole-container assignment (`@a = ...`, `%h = ...`) into a local the
# routine declares checks the routine's own declaration, never a caller's
# same-named typed variable (ADR-13757, ADR-14632 §2.6).

plan 12;

sub own-hash() { my %h; %h = (a => 'x'); %h<a> }
sub typed-hash-caller() { my Int %h; own-hash() }
is typed-hash-caller(), 'x', 'untyped %h ignores a caller\'s Int %h';

sub own-array() { my @a; @a = <x y>; @a }
sub typed-array-caller() { my Int @a; own-array() }
is typed-array-caller().join(','), 'x,y', 'untyped @a ignores a caller\'s Int @a';

sub bound-array() { my @x; my @b := @x; @b = <x>; @b }
sub typed-bound-caller() { my Int @b; bound-array() }
is typed-bound-caller().join(','), 'x', 'a :=-bound @b writes through without the caller\'s type';

sub bound-hash() { my %x; my %b := %x; %b = (k => 'v'); %b }
sub typed-bound-hash-caller() { my Int %b; bound-hash() }
is typed-bound-hash-caller()<k>, 'v', 'a :=-bound %b writes through without the caller\'s type';

sub eval-hash() { my %h; EVAL q[%h = (a => 'y')]; %h }
sub typed-eval-caller() { my Int %h; eval-hash() }
is typed-eval-caller()<a>, 'y', 'an EVAL\'d whole-hash store sees the routine\'s own %h';

my Int @t = 1, 2;
@t = 3, 4;
is @t.join(','), '3,4', 'a typed @t accepts matching values';
throws-like { @t = <a> }, X::TypeCheck::Assignment, 'a typed @t refuses a Str';

my Int %th{Str} = a => 1;
%th = b => 2;
is %th<b>, 2, 'a typed object hash accepts a matching value';
throws-like { %th = c => 'x' }, X::TypeCheck::Assignment, 'a typed object hash refuses a Str value';

sub own-typed() { my Int @q; @q = 5; @q }
is own-typed().join(','), '5', 'a routine\'s own Int @q accepts an Int';

sub own-typed-refuses() {
    my Int @q;
    my $err;
    try { @q = 'z'; CATCH { default { $err = .^name } } }
    ($err, @q.elems)
}
my ($err, $elems) = own-typed-refuses();
is $err, 'X::TypeCheck::Assignment', 'a routine\'s own Int @q refuses a Str';
is $elems, 0, 'the refused store leaves @q empty';
