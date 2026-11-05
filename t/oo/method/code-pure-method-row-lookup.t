use Test;

# ADR-11276: pure code methods use the same class/MRO rows as ordinary
# method dispatch, while interpreter methods retain their own implementations.
my @values =
    'block' => { 42 },
    'pointy' => -> $x { $x },
    'sub' => sub { 42 },
    'method' => method { 42 },
    'regex' => /a/,
    'builtin' => &say;

for @values -> $entry {
    my $value = $entry.value;
    my $copy = $value.clone;
    is $copy.^name, $value.^name, "{$entry.key}: clone preserves its code class";
    throws-like { $value.Capture }, X::Cannot::Capture,
        "{$entry.key}: Capture retains the Code refusal";
    my $method = 'clone';
    is $value."$method"().^name, $value.^name,
        "{$entry.key}: dynamic clone reaches the same row";
    lives-ok { $value.raku }, "{$entry.key}: interpreter-only rendering remains available";
}

my $closure = -> { state $n = 0; ++$n };
my $fork = $closure.clone;
is $closure(), 1, 'original closure starts its own state';
is $fork(), 1, 'clone starts independent state';
is $closure(), 2, 'original state advances independently';
is $fork(), 2, 'cloned state advances independently';

my &with-args = sub ($x, $y) { $x + $y };
is &with-args.arity, 2, 'an interpreter-only Code row retains its binding metadata';

done-testing;
