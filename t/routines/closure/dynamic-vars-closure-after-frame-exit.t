use Test;

# A dynamic variable (`my $*x`) belongs to the call stack: a closure called
# after the frame that bound it exited must not see it (#15669). Expected values
# are rakudo 2026.09's.

plan 6;

sub peek { $*X // "undef" }

sub make-closure { my $*X = 1; return { peek() } }
my &g = make-closure();
is g(), "undef", 'a closure returned from the binding frame no longer sees it';

sub inside { my $*X = 2; my &c = { peek() }; c() }
is inside(), 2, 'called inside the binding frame it still does';

sub map-inside { my $*X = 3; my @r = (1, 2).map({ peek() }); @r.raku }
is map-inside(), '[3, 3]', 'a map run inside the binding frame sees it';

sub lazy-map { my $*X = 4; return (1, 2).map({ peek() }) }
is lazy-map().list.raku, '("undef", "undef")', 'a lazy map pulled after the frame exited does not';

sub lazy-gather { my $*X = 5; return gather { take peek() } }
is lazy-gather().list.raku, '("undef",)', 'neither does a gather';

sub caller-binds { my $*X = 6; g() }
is caller-binds(), 6, 'a closure called by a frame that binds it sees the caller\'s binding';
