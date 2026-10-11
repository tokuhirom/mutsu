use v6;
use Test;
use nqp;

# BOOTInt / BOOTNum / BOOTStr are KnowHOW types outside Mu, as the BOOT
# collection types are (ADR-12666, #12670). Answers checked against Rakudo.
plan 21;

my $bi := nqp::box_i(1, nqp::bootint());
my $bn := nqp::box_n(1.5e0, nqp::bootnum());
my $bs := nqp::box_s('x', nqp::bootstr());

is nqp::istype($bi, Mu), 0, 'a BOOTInt box is not a Mu';
is nqp::istype(nqp::bootint(), Mu), 0, 'the BOOTInt type object is not a Mu';
is nqp::istype($bi, nqp::bootint()), 0, 'a KnowHOW type has no type check, not even its own';
is nqp::istype(nqp::list(), nqp::bootarray()), 0, '... the same for a raw VMArray';
is $bi.HOW.^name, 'KnowHOW', 'its HOW is KnowHOW';
is nqp::bootint().HOW.^name, 'KnowHOW', 'the type object\'s HOW is KnowHOW';
is $bi.^name, 'BOOTInt', '.^name';
is $bi.REPR, 'P6int', 'BOOTInt REPR';
is $bn.REPR, 'P6num', 'BOOTNum REPR';
is $bs.REPR, 'P6str', 'BOOTStr REPR';
is nqp::bootstr().REPR, 'P6str', 'type object REPR';
ok nqp::eqaddr(nqp::what($bi), nqp::bootint()), 'nqp::what is the type object';
ok $bi ~~ Mu, 'smartmatch against Mu accepts it (Mu.ACCEPTS)';
nok $bi ~~ Any, 'but it is not an Any';
nok $bi ~~ Int, 'nor an Int';
throws-like { $bi.Int }, X::Method::NotFound, message => /"No such method 'Int' for invocant of type 'BOOTInt'"/,
    'a method call on a box dies';
throws-like { nqp::bootint().new }, X::Method::NotFound, message => /"No such method 'new' for invocant of type 'BOOTInt'"/,
    'a method call on the type object dies';
throws-like { $bs.chars }, X::Method::NotFound, message => /"No such method 'chars'"/,
    'a BOOTStr box has no Str methods';
my $c := nqp::create(nqp::bootnum());
is $c.^name, 'BOOTNum', 'create of a BOOT scalar type makes a box';
is nqp::unbox_n($c), 0e0, '... holding zero';
is nqp::hllize($bi).^name, 'Int', 'hllize still gives the high-level Int';
