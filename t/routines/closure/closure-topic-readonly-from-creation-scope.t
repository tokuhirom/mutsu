use Test;

# A block called with no argument writes the `$_` of the scope it was created
# in, so that scope decides whether `$_` is readonly -- not the routine that
# calls the block (#19067).

plan 5;

sub call-it(&b) { (try { b(); 'assigned' }) // 'died' }

my @results;
for 1, 2 { @results.push: call-it({ $_ = 0 }) }
is-deeply @results, ['died', 'died'], 'a for topic over values stays readonly in a passed block';

@results = ();
for 1, 2 { @results.push: (try { $_ = 0; 'assigned' }) // 'died' }
is-deeply @results, ['died', 'died'], 'and inline, as before';

my @a = 1, 2;
for @a { call-it({ $_ = 9 }) }
is-deeply @a, [9, 9], 'a for topic over a container stays writable in a passed block';

sub ro-caller($x) { my $blk = { $_ = 3 }; given my $v = 1 { call-it($blk) } }
is ro-caller(1), 'assigned', 'a writable creation-scope topic wins over the caller';

sub passes-ok(&b) { b() }
for 1 { passes-ok({ ok True, 'a block that does not write $_ is unaffected' }) }
