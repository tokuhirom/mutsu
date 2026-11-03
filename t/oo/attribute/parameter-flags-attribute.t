use v6;
use nqp;
use Test;

# A Parameter keeps its boolean properties in one Int bit field, `$!flags`,
# reachable through the MOP (Deps' `is injected` trait pokes it there).
# The expected words below were read off rakudo.

plan 20;

sub flags-of(Parameter $p) {
    $p.^attributes.first(*.name eq '$!flags').get_value($p)
}

my $p = (-> $x { }).signature.params[0];
my $attr = $p.^attributes.first(*.name eq '$!flags');
ok $attr.defined, 'Parameter.^attributes exposes $!flags';
isa-ok $attr.get_value($p), Int, 'its value is an Int';

sub f($a, Int:D $i, $j is rw, $k is copy, $l is raw, %m, @n, \g, &h, $b?, :$c!, :$d, :$e = 5, *@s) { }
my @want = 128, 131200, 384, 640, 1152, 8320, 4224, 1152, 33554560, 2176, 128, 2176, 1050752, 4232;
is-deeply &f.signature.params.map(&flags-of).List, @want.List, 'flag words of positional/named/slurpy parameters';

sub g(:$x, **@y) { }
is-deeply &g.signature.params.map(&flags-of).List, (2176, 4256), 'a **@ slurpy sets SLURPY_LOL';

sub h(+@z, *%w, |c) { }
is-deeply &h.signature.params.map(&flags-of).List, (16781440, 8336, 33920), '+@, *% and | parameters';

sub k(Str() $q, Any:U $r, $s where * > 1, :@t) { }
is-deeply &k.signature.params.map(&flags-of).List, (67108992, 65664, 128, 6272), 'coercion, :U, where, optional named array';

class C { method m($self: $x) { } }
is-deeply C.^lookup('m').signature.params.map(&flags-of).List, (192, 128, 8336), 'invocant and the implicit %_ of a method';

is nqp::const::SIG_ELEM_IS_OPTIONAL, 2048, 'SIG_ELEM_IS_OPTIONAL';
is nqp::const::SIG_ELEM_IS_RW, 256, 'SIG_ELEM_IS_RW';
is nqp::const::SIG_ELEM_SLURPY_ONEARG, 16777216, 'SIG_ELEM_SLURPY_ONEARG';
is nqp::const::SIG_ELEM_CODE_SIGIL, 33554432, 'SIG_ELEM_CODE_SIGIL';
is nqp::const::SIG_ELEM_IS_NOT_READONLY, 1792, 'composite SIG_ELEM_IS_NOT_READONLY';

# The bits feed back into what the parameter reports.
my $q = (-> $x { }).signature.params[0];
nok $q.optional, 'a required parameter is not optional';
my $f = $q.^attributes.first(*.name eq '$!flags');
$f.set_value($q, $f.get_value($q) +| nqp::const::SIG_ELEM_IS_OPTIONAL);
ok $q.optional, 'setting SIG_ELEM_IS_OPTIONAL makes .optional true';
is $f.get_value($q), 2176, 'and the word reads back';
$f.set_value($q, $f.get_value($q) +& +^nqp::const::SIG_ELEM_IS_OPTIONAL);
nok $q.optional, 'clearing the bit makes it required again';

# A trait_mod for Parameter can set the bit (the shape Deps uses).
multi trait_mod:<is>(Parameter $param, :$injected!) {
    my $fl = $param.^attributes.first: *.name eq q"$!flags";
    $fl.set_value: $param, $fl.get_value($param) +| nqp::const::SIG_ELEM_IS_OPTIONAL;
}
sub with-trait($a, $b is injected) { }
ok &with-trait.signature.params[1].optional, 'a trait that sets the bit changes .optional';
nok &with-trait.signature.params[0].optional, 'other parameters are untouched';
is flags-of(&with-trait.signature.params[1]), 2176, 'the trait-mutated Parameter keeps its flags';
is flags-of(&with-trait.signature.params[0]), 128, 'the plain one keeps its own';
