use Test;
use lib 't/lib';
use ClientScope;

# Distribution: Prompt::Expand (CLIENT::<$*INDEX> in a signature default).
plan 4;

is client-index(), 0, 'CLIENT::<$*X> is Nil when the caller has no such variable';

my $*CLIENT-INDEX = 42;
is client-index(), 42, 'a signature default reads the dynamic variable of the calling script';
is client-direct(), 42, 'CLIENT::<$*X> in a routine body reads the calling script';
is client-via-helper(), 42, 'a call from the same package still reaches the script';
