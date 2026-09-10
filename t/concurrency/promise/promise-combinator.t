use Test;

plan 7;

my $gate = Promise.new;
my $all-seq = Promise.allof((1..1).map({ start { await $gate; 42 } }));
nok $all-seq.Bool, 'allof over Seq waits unresolved promises';
$gate.keep(True);
ok $all-seq.result, 'allof over Seq resolves to true';

my $all-empty = Promise.allof(do my @promises);
is $all-empty.status, Kept, 'allof of an empty list is immediately Kept';

my $any-empty = Promise.anyof();
is $any-empty.status, Kept, 'anyof with no arguments is immediately Kept';

throws-like { Promise.allof(42) }, X::Promise::Combinator, 'allof rejects non-Promise arguments';
throws-like { Promise.anyof(42) }, X::Promise::Combinator, 'anyof rejects non-Promise arguments';

my $seen = [];
Promise.allof(start { cas $seen, -> @current { flat @current, 1 } }).result;
is ~$seen, '1', 'allof.result syncs cas updates from worker promises';
