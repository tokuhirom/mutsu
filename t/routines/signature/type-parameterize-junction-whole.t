use Test;

# A Junction handed to `Type[...]` with a user `^parameterize` reaches the
# metamethod unthreaded when its parameter binds it whole (`|args`, `Mu`), and
# a proto/multi pair typed by the minted subset still dispatches on it.
# Found with the `Kind` distribution (Kind[A | B]).
plan 12;

class P {
    method ^parameterize(|args) is raw { args.list[1].^name }
}
is P[Int|Str], 'Junction', '|args takes the junction whole';

my @seen;
class Q {
    method ^parameterize(Mu $obj, Any $args) is raw { @seen.push($args.^name); Int }
}
Q[Int|Str];
is @seen.sort.join(','), 'Int,Str', 'an Any parameter still threads';

class K {
    method ^parameterize(|args) is raw { self.kind: |args }
    method ^kind(Mu $obj is raw, Mu \T) is raw { T.^name }
}
is K[Int|Str], 'Junction', 'self.kind: |args does not thread into a Mu parameter';

# proto method {*} binds by the proto's own parameters
my class Refine is Mu {
    proto method CALL-ME(Mu) {*}
    multi method CALL-ME(Mu $topic is raw) { 'plain' }
    multi method CALL-ME(Junction:D $junction is copy) { 'junction' }
}
is Refine(Int|Str), 'junction', 'proto Mu {*} keeps the junction for the Junction:D candidate';

sub jd(Junction:D $j) { 'whole' }
is jd(1|2), 'whole', 'Junction:D parameter takes the junction whole';

# a multi ACCEPTS without a proto threads, as rakudo does (and only that name)
class R is Mu { multi method ACCEPTS(Mu \K) { 'got ' ~ K.^name } }
is R.ACCEPTS(Int|Str).gist, 'any(got Int, got Str)', 'multi ACCEPTS autothreads';
class R3 is Mu { method ACCEPTS(Mu \K) { 'got ' ~ K.^name } }
is R3.ACCEPTS(Int|Str), 'got Junction', 'single ACCEPTS takes the junction whole';
class R2 is Mu { multi method other(Mu \K) { 'got ' ~ K.^name } }
is R2.other(Int|Str), 'got Junction', 'other multi methods take the junction whole';

# a minted subset named by free text still type-checks (name holds ', ' and '(')
for 'Foo[any(A, B)]', 'Foo[x, y]', 'Foo(A)' -> $name {
    my $obj := Metamodel::SubsetHOW.new_type: :$name, :refinee(Mu),
        :refinement(sub (Mu $t) { True });
    ok Int ~~ $obj, "subset named '$name' accepts via its refinement";
}

# Metamodel::Primitives takes Mu arguments: no threading
ok Metamodel::Primitives.is_type(Int|Str, Mu), 'Primitives.is_type is not threaded';
