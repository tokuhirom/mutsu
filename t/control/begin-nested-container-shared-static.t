use Test;

# An `@`/`%` lexical that a nested BEGIN (or INIT/CHECK) touched is bound to
# its static cell on every scope entry, so every frame shares the one object,
# as rakudo's pad clone does (#10333). A scalar is copied.

plan 13;

sub arr { my @a; BEGIN @a.push(1); @a.push(2); @a.join(',') }
is arr(), '1,2', 'first call sees the BEGIN push';
is arr(), '1,2,2', 'second call shares the same array';

sub hsh { my %h; BEGIN %h<a> = 1; %h<b>++; %h.sort.map({ "{.key}={.value}" }).join(',') }
is hsh(), 'a=1,b=1', 'first call sees the BEGIN store';
is hsh(), 'a=1,b=2', 'second call shares the same hash';

sub with-init { my @a = 5; BEGIN @a.push(1); @a.push(2); @a.join(',') }
is with-init(), '5,2', 'an initializer assigns into the shared array';
is with-init(), '5,2', '... on every entry';

sub typed { my Int @a; BEGIN @a.push(1); @a.push(2); "{@a.join(',')}/{@a.of.^name}" }
is typed(), '1,2/Int', 'a typed array keeps its element type';
is typed(), '1,2,2/Int', '... and is shared';

{
    my @seen;
    for ^2 { my @a; BEGIN @a.push(7); @a.push($_); @seen.push: @a.join(',') }
    is @seen.join(' '), '7,0 7,0,1', 'a loop body shares the array across iterations';
}

{
    my $c = { my @b; BEGIN @b.push(0); @b.push(1); @b.elems };
    is "{$c()}{$c()}", '23', 'a closure body shares the array across calls';
}

sub scalar { my $s; BEGIN $s = 1; $s++; $s }
is scalar(), 2, 'a scalar starts from its static value';
is scalar(), 2, '... copied on each entry';

sub via-init { my @a; INIT @a.push(1); @a.push(2); @a.join(',') }
via-init();
is via-init(), '1,2,2', 'an INIT phaser shares the array the same way';
