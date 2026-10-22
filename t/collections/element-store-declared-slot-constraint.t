use Test;

# An element / nested / push-family store into a variable the running code
# declares takes its constraint from that declaration, never from a
# same-named variable of a calling frame (ADR-13757).

plan 12;

sub elem-store()   { my @a; @a[0] = 's'; @a[2] = 't'; @a.elems }
sub hash-store()   { my %h; %h<k> = 's'; %h.elems }
sub nested-store() { my @a; @a[0][1] = 's'; @a[0].elems }
sub deep-store()   { my @a; @a[0][1][2] = 's'; @a[0][1].elems }
sub nested-hash()  { my %h; %h<a><b> = 's'; %h<a>.elems }
sub push-store()   { my @a; @a.push('s'); @a.push('t', 'u'); @a.elems }
sub compound()     { my @a = 1, 2; @a[1] += 5; @a[1] }

sub typed-caller-int-array(&f) { my Int @a; my Int %h; f() }

is typed-caller-int-array(&elem-store),   3, 'element store ignores a caller\'s typed array';
is typed-caller-int-array(&hash-store),   1, 'hash store ignores a caller\'s typed hash';
is typed-caller-int-array(&nested-store), 2, 'nested store ignores a caller\'s typed array';
is typed-caller-int-array(&deep-store),   3, 'deep store ignores a caller\'s typed array';
is typed-caller-int-array(&nested-hash),  1, 'nested hash store ignores a caller\'s typed hash';
is typed-caller-int-array(&push-store),   3, 'push family ignores a caller\'s typed array';
is typed-caller-int-array(&compound),     7, 'compound store ignores a caller\'s typed array';

# The declaring code's own constraint still applies on every path.
{
    my Int @a;
    throws-like { @a[0] = 's' }, X::TypeCheck::Assignment, 'element store checks its own type';
    my Int @b;
    throws-like { @b.push('s') }, X::TypeCheck, 'push checks its own type';
    my Int @c;
    throws-like { @c[0][1] = 's' }, X::TypeCheck::Assignment,
        'nested store into a typed array is refused';
}

# `push` routed through the generic method-call path wraps native ints by the
# declaring slot's element type, not a caller's.
sub native-push() { my uint8 @e; @e.push(1, 300, 2); @e.join(',') }
sub native-caller(&f) { my str @e; f() }
is native-caller(&native-push), '1,44,2', 'native int push wraps by its own declaration';
{
    my uint8 @e;
    my $m = 'push';
    @e."$m"(1, 300);
    is @e.join(','), '1,44', 'dynamic-name push wraps by its own declaration';
}
