use Test;

# A dynamic variable (`my $*x`) takes a shared cell at its declaration, so a
# routine called in its scope that writes `$*x` by name writes the declaring
# frame's variable, without a return merge carrying the write back
# (ADR-12529 §3, slice 39). Built-in readers of a dynamic (`$*TOLERANCE`,
# `$*SCHEDULER`) see through the cell. Checked against rakudo.

plan 7;

sub w() { $*x = 5 }
sub declares-x() { my $*x = 99; w(); $*x }
is declares-x(), 5, 'a callee assigns the caller-declared dynamic';

sub inc() { $*n++ }
sub counts() { my $*n = 0; inc(); inc(); $*n }
is counts(), 2, 'a callee increments it, twice';

sub push-it() { @*log.push('p') }
sub logs() { my @*log; push-it(); push-it(); @*log.join(',') }
is logs(), 'p,p', 'a dynamic array pushed by a callee';

sub set-h() { %*h<k> = 1 }
sub hashes() { my %*h; set-h(); %*h.keys.join }
is hashes(), 'k', 'a dynamic hash stored by a callee';

sub shadows() { my $*x = 2; w() }
sub outer() { my $*x = 1; shadows(); $*x }
is outer(), 1, 'a nearer declaration takes the write';

sub tolerant() { my $*TOLERANCE = 0.1; so 1 =~= 1.05 }
ok tolerant(), 'a declared $*TOLERANCE is honored';

sub scheduled() { my $*SCHEDULER = CurrentThreadScheduler.new; await start { 42 } }
is scheduled(), 42, 'a declared $*SCHEDULER is used';
