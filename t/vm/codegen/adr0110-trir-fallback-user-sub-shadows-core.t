# #9288: a statically linked `CallTrir` site whose bind declines re-dispatches
# the call by name. That fallback used to go through `call_function`, which
# matches core routine names before it consults the registry, so a user
# `sub copy(...)` whose bind declined ran the core `copy` instead
# ("copy requires a destination path"). The fallback must resolve the name
# the way the untyped call does: the user's declaration shadows the core
# routine.
#
# A sigilless parameter always declines the TRIR bind for a caller variable
# (it would bind the container), which is what reaches the fallback here.
use Test;
use nqp;

plan 4;

sub copy(Uni:D \codes) { nqp::atpos_i(codes, 0) }
my $codes := nqp::strtocodes("abc", nqp::const::NORMALIZE_NFC, nqp::create(NFC));
is copy($codes), 97, 'declined bind reaches the user sub, not core copy';

sub words(\x) { "user words: " ~ x }
my $s = "a b";
is words($s), 'user words: a b', 'same for another core name (words)';

sub not-core(\x) { x ~ "!" }
is not-core($s), 'a b!', 'a name with no core routine still works';

# A type mismatch still declines to the fallback, and must raise the user
# sub's binding error rather than run the core routine.
sub dir(Int $n) { $n }
my $str = "x";
dies-ok { dir($str) }, 'type-mismatched call to a user sub shadowing core dies';
