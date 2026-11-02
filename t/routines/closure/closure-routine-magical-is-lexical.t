use Test;

# `&?ROUTINE` is a lexical: a block means the routine it is written in, even
# when it runs after that routine returned (a lazy `map`) or from another
# routine. From the Deepgrep ecosystem distribution (#16062).

plan 9;

sub lazy-name($n) { (1, 2).map: -> $i { &?ROUTINE.name } }
is-deeply lazy-name(0).list, ("lazy-name", "lazy-name"), 'a lazy map block names its routine';

sub lazy-call($n) { (1, 2).map: -> $i { &?ROUTINE.($n - 1) if $n > 0; $i } }
is-deeply lazy-call(1).list, (1, 2), '&?ROUTINE.() from a lazy block recurses into the routine';

sub lazy-call3($n) { (1, 2).map: -> $i { $n > 0 ?? &?ROUTINE($n - 1).elems !! $i } }
is-deeply lazy-call3(1).list, (2, 2), '&?ROUTINE(...) from a lazy block recurses into the routine';

sub nested { (1, 2).map({ -> { &?ROUTINE.name } }).map({ $_() }) }
sub other { nested().join(",") }
is other(), 'nested,nested', 'a block nested in a lazy block keeps the outer routine';

sub mk { -> { &?ROUTINE.name } }
sub caller-of(&f) { f() }
is caller-of(mk()), 'mk', 'a closure run from another routine means its own routine';

sub bare { my $b = { &?ROUTINE.name }; $b }
is bare()(), 'bare', 'a bare block run after its routine returned';

sub anon { my $s = sub { &?ROUTINE.name }; $s }
is anon()(), '', 'an anonymous sub is its own routine';

sub recurse($n) { $n <= 0 ?? 0 !! (1,).map({ $n + &?ROUTINE($n - 1) }).sum }
is recurse(3), 6, 'recursion through a block inside the routine';

sub direct { &?ROUTINE.name }
is direct(), 'direct', 'a direct read is unchanged';
