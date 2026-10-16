use Test;

# `Routine`'s `is_dispatcher`, `dispatcher`, `multi` and `package`, and
# `Code.set_name` on a routine and a regex, are rows of the method table,
# found by their owner (ADR-11276 §9.121).

plan 16;

multi sub mm(Int $x) { 1 }
multi sub mm(Str $x) { 2 }
sub plain() { 3 }
proto sub pp(|) {*}
multi sub pp(Int) { 4 }

ok &mm.is_dispatcher, 'a multi is its dispatcher';
nok &mm.candidates[0].is_dispatcher, 'a candidate is no dispatcher';
ok &mm.candidates[0].multi, 'a candidate is multi';
nok &plain.is_dispatcher, 'a plain sub is no dispatcher';
nok &plain.multi, 'a plain sub is not multi';
is &mm.candidates[0].dispatcher.candidates.elems, 2,
    "a candidate's dispatcher has every candidate";
nok &plain.dispatcher.defined, 'a plain sub has no dispatcher';
is &pp.candidates[0].dispatcher.name, 'pp', "a proto's candidate's dispatcher";

is &plain.package.^name, 'GLOBAL', 'package of a sub';
module M { our sub q() { 1 } }
is &M::q.package.^name, 'M', 'package of a qualified handle';
class C {
    method m() { }
    multi method n(Int) { }
    multi method n(Str) { }
}
is C.^lookup('m').package.^name, 'C', 'package of a method';
ok C.^find_method('n').is_dispatcher, 'a multi method is its dispatcher';

my $r = /abc/;
$r.set_name('foo');
is $r.name, 'foo', 'set_name renames the regex';
my &s = sub { };
&s.set_name('bar');
is &s.name, 'bar', 'set_name renames the sub';

dies-ok { 42.package }, 'an Int has no package';
ok Routine.^can('is_dispatcher'), '.^can sees is_dispatcher';
