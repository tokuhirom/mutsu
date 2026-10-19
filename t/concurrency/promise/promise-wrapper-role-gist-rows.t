use Test;

# The `gist` of `X::Promise::Broken`, `X::React::Died` and `X::Await::Died`,
# the roles mixed into an exception that `Promise.result`, a dying `react` or
# `await` rethrows, is a row of the role, however the call is made; every
# other method still answers the wrapped exception (ADR-11276 §9.199; checked
# against Rakudo).

plan 11;

my $p = Promise.new;
$p.break("oh no");
try $p.result;
my $e = $!;
is $e.^name, 'X::AdHoc+{X::Promise::Broken}', 'the cause carries the role';
is $e.gist.lines[0], 'Tried to get the result of a broken Promise', 'its gist';
my $m = 'gist';
is $e."$m"().lines[0], 'Tried to get the result of a broken Promise',
    'through a dynamic method name';
ok $e.gist.contains('Original exception:'), 'the gist names the original exception';
ok $e.gist.contains('oh no'), '... and includes its gist';
is $e.message, 'oh no', 'message is the cause\'s';
is $e.Str, 'oh no', 'Str is the cause\'s';
ok $e.can('gist'), 'it can gist';

try { react { whenever Supply.from-list(1) { die "boom" } } };
is $!.gist.lines[0], 'A react block:', 'a dying react block';
ok $!.gist.contains('Died because of the exception:'), '... explains the exception';

my $q = start { die "late" };
try await $q;
is $!.gist.lines[0], 'An operation first awaited:', 'a broken await';
