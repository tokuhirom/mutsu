use Test;

plan 4;

my $called = 0;
my $handle = &dir.wrap(sub ($path = '.') {
    $called++;
    callsame;
});

ok &dir.can('wrap'), 'a named Routine handle reports wrap support';
ok dir('.').elems >= 0, 'a wrapped builtin routine remains callable by name';
is $called, 1, 'a wrapper installed through a Routine handle intercepts a named call';
ok &dir.unwrap($handle), 'a Routine handle removes its wrapper by handle';
