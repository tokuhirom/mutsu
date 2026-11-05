use Test;
use lib 't/lib';
use CoerceRelPkg;

plan 2;

# A coercion-typed named leaf of a sub-signature (`Map (Column::List(Any) :$pb)`)
# that is omitted must bind a type object the typed attribute accepts.
my $d;
lives-ok { $d = CoerceRelPkg::mk-def() }, 'omitted sub-signature leaf passes the typed attribute check';
ok $d.pb.defined.not, 'the attribute holds a type object';
