use Test;

# From the Stomp distribution: `.head(N)` over a `supply { whenever ... }`
# block must tap it lazily; it used to run the block eagerly and hang.

plan 6;

my $s = Supplier.new;
my $sup = supply { whenever $s.Supply -> $d { emit $d } };
my $first = $sup.head(1);
my @got;
my $done = False;
$first.tap({ @got.push($_) }, done => { $done = True });
$s.emit('a');
$s.emit('b');
is-deeply @got, ['a'], 'head(1) passes only the first value';
ok $done, 'head(1) signals done after the count';

my $s2 = Supplier.new;
my $p = supply { whenever $s2.Supply -> $d { emit $d } }.head(1).Promise;
is $p.status, Planned, 'promise planned before any emission';
$s2.emit('x');
is $p.status, Kept, 'promise kept after the first emission';
is $p.result, 'x', 'promise carries the value';

my $s3 = Supplier.new;
my $p3 = supply { whenever $s3.Supply -> $d { emit $d } }.grep(* eq 'x').head(1).Promise;
$s3.emit('y');
$s3.emit('x');
is $p3.status, Kept, 'grep.head(1) over a supply block';
