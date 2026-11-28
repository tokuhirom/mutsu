use Test;

# `IO::Path.resolve-inside($io)`, added in rakudo 2026.11: the invocant
# resolved completely when it lies strictly inside the existing directory
# `$io`, otherwise Nil (#19274).

plan 11;

my $root = $*TMPDIR.add("mutsu-resolve-inside-{$*PID}");
$root.add("a/b").mkdir;
LEAVE { $root.add("a/b").rmdir; $root.add("a").rmdir; $root.rmdir }

my $inside = $root.add("a/b").resolve-inside($root);
isa-ok $inside, IO::Path, 'a path inside the directory resolves';
is $inside.Str, $root.resolve(:completely).add("a/b").Str, 'to its complete resolution';
ok $inside.is-absolute, 'which is absolute';

is $root.add("a/../a/b").resolve-inside($root).Str,
   $root.resolve(:completely).add("a/b").Str, 'a .. inside the directory is resolved first';

ok $root.resolve-inside($root) === Nil, 'the directory itself is not inside itself';
ok $root.add("a/../..").resolve-inside($root) === Nil, 'a path escaping through .. is Nil';
is $root.add("missing").resolve-inside($root).Str,
   $root.resolve(:completely).add("missing").Str,
   'a missing last segment still resolves, as .resolve(:completely) allows';
ok $root.add("missing/deeper").resolve-inside($root) === Nil, 'a path that does not resolve is Nil';
ok $root.add("a/b").resolve-inside($root.add("missing")) === Nil,
   'an $io that is not an existing directory gives Nil';

throws-like { $root.resolve-inside("x") }, X::TypeCheck::Binding::Parameter,
    message => /'$io'/, 'a Str argument is a type check failure';

ok IO::Path.^lookup('resolve-inside'), 'the method is visible to .^lookup';
