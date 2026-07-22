use v6;
use Test;

plan 7;

# Promise.raku renders the Rakudo attribute form, not a bare `Promise(Status)`.
my $planned = 'Promise.new(scheduler => ThreadPoolScheduler.new(uncaught_handler => Callable), status => PromiseStatus::Planned)';
my $kept    = 'Promise.new(scheduler => ThreadPoolScheduler.new(uncaught_handler => Callable), status => PromiseStatus::Kept)';
my $broken  = 'Promise.new(scheduler => ThreadPoolScheduler.new(uncaught_handler => Callable), status => PromiseStatus::Broken)';

my $p = Promise.new;
is $p.raku, $planned, 'planned Promise.raku';
is $p.gist, $planned, 'Promise has no custom gist, so .gist is the .raku form';

$p.keep(42);
is $p.raku, $kept, 'kept Promise.raku';

my $b = Promise.new;
$b.break('nope');
is $b.raku, $broken, 'broken Promise.raku';

# Nested inside containers, the same form is used.
is [$p].raku, "[$kept]", 'Promise nested in an array .raku';
is [$p].gist, "[$kept]", 'Promise nested in an array .gist';
is (:p($b),).hash.raku, "\{:p($broken)}", 'Promise nested in a hash .raku';
