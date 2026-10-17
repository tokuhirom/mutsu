use Test;

# An element store a routine or a closure makes to a variable of its lexical
# scope is checked against that variable's own element type, never against a
# same-named, typed variable of whichever routine called it (ADR-13757).
# Checked against rakudo.

plan 10;

my @a;
sub push-elem() { @a[0] = 'x'; @a.push('y'); @a.elems }
sub typed-array-caller() { my Int @a; push-elem() }
is typed-array-caller(), 2, 'an element store ignores a caller my Int @a';

my %h;
sub store-key() { %h<k> = 'x'; %h.elems }
sub typed-hash-caller() { my Int %h; store-key() }
is typed-hash-caller(), 1, 'a hash element store ignores a caller my Int %h';

my @c;
sub closure-store() { my $c = { @c[0] = 's' }; $c(); @c[0] }
sub closure-caller() { my Int @c; closure-store() }
is closure-caller(), 's', 'a closure\'s element store ignores a caller my Int @c';

sub own-local() { my @w; my $c = { @w[0] = 's' }; $c(); @w[0] }
sub own-local-caller() { my Int @w; own-local() }
is own-local-caller(), 's', 'a closure over a routine local ignores the caller';

my $s = 1;
sub closure-scalar() { my $c = { $s = 's' }; $c(); $s }
sub scalar-caller() { my Int $s = 2; closure-scalar() }
is scalar-caller(), 's', 'a closure\'s scalar store ignores a caller my Int $s';

my Int @typed;
sub store-typed() { @typed[0] = 's' }
sub plain-array-caller() { my @typed; store-typed() }
throws-like { plain-array-caller() }, X::TypeCheck,
    'the array\'s own element type still applies';

my Int %typed;
sub store-typed-key() { %typed<k> = 's' }
sub plain-hash-caller() { my %typed; store-typed-key() }
throws-like { plain-hash-caller() }, X::TypeCheck,
    'the hash\'s own value type still applies';

my Int @ctyped;
sub closure-typed() { my $c = { @ctyped[0] = 's' }; $c() }
sub closure-plain-caller() { my @ctyped; closure-typed() }
throws-like { closure-plain-caller() }, X::TypeCheck,
    'a closure checks the variable\'s own type';

sub own-typed() { my Int @w; my $c = { @w[0] = 's' }; $c() }
throws-like { own-typed() }, X::TypeCheck, 'a closure over a typed routine local';

sub alias(\x) { x[0] = 's' }
sub alias-caller() { my Int @q; alias(@q) }
throws-like { alias-caller() }, X::TypeCheck,
    'a sigilless parameter checks the bound array\'s type';
