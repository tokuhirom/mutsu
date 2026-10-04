use v6;
use Test;

plan 14;

# A `&` entry of the `PROCESS::` stash is the process-level dynamic `&*name`
# (#9881). Bound with `:=`, or installed by the setting (`&chdir`), it holds
# the code object itself, so assigning to it dies; assigning to a fresh key
# gives it a Scalar that later assignments store into.

sub msg(&code) {
    try { code() };
    $! ?? $!.message !! 'assigned';
}

isa-ok PROCESS::<&chdir>, Sub, 'the stash holds the process-level &*chdir';
is msg({ PROCESS::<&chdir> = sub ($p) { } }), 'Cannot assign to an immutable value',
    'assigning to the installed &chdir dies';
isa-ok PROCESS::<&chdir>, Sub, '... and leaves it in place';
my $key = '&chdir';
is msg({ PROCESS::{$key} = sub ($p) { } }), 'Cannot assign to an immutable value',
    'the same through a computed key';

is msg({ PROCESS::<&foo> = sub { 42 } }), 'assigned', 'assigning to a fresh key works';
is PROCESS::<&foo>(), 42, 'the stash entry calls the assigned routine';
is &*foo(), 42, '... and so does the dynamic &*foo';
PROCESS::<&foo> = sub { 43 };
is PROCESS::<&foo>(), 43, 'a second assignment stores into the same entry';
sub calls-foo { &*foo() }
is calls-foo(), 43, 'a routine sees it as &*foo';

PROCESS::<&bar> := sub { 'bar' };
is PROCESS::<&bar>(), 'bar', 'binding a fresh key works';
is msg({ PROCESS::<&bar> = sub { 'other' } }), 'Cannot assign to an immutable value',
    'assigning to a bound entry dies';
is PROCESS::<&bar>(), 'bar', '... and leaves the bound routine';
is (await start { &*bar() }), 'bar', 'another thread sees the bound &*bar';

PROCESS::<&chdir> := sub ($p) { 'replaced' };
is &*chdir('/'), 'replaced', 'binding replaces the installed &chdir';
