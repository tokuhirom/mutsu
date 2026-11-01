use Test;

# The per-call answers a method call caches (the receiver has no built-in
# shape; nothing is wrapped) must not outlive the facts they depend on
# (ADR-14633). Many classes are called in turn so any per-symbol table is
# reused and overwritten, and a method is wrapped after plain calls to it.

plan 7;

my @classes;
for 1..300 -> $i {
    my $c := Metamodel::ClassHOW.new_type(:name("Churn$i"));
    $c.^add_method('who', method () { $i });
    $c.^compose;
    @classes.push($c);
}

my $sum = 0;
for 1..3 {
    for @classes -> $c {
        $sum += $c.new.who;
    }
}
is $sum, 3 * (1..300).sum, 'every instance answers its own class method, round after round';

class Plain { method m($x) { $x + 1 } }
my $p = Plain.new;
is $p.m(1), 2, 'a plain call before any wrap';
is $p.m(2), 3, 'and again, so the plain-call answers are cached';

my $handle = Plain.^find_method('m').wrap(-> $self, $x { callsame() * 10 });
is $p.m(1), 20, 'a wrap installed after plain calls is honoured';
is $p.m(2), 30, 'and again';

Plain.^find_method('m').unwrap($handle);
is $p.m(1), 2, 'unwrapping restores the plain call';

# A builtin-shaped instance still gets its builtin methods after the churn.
is (1, 2, 3).Seq.elems, 3, 'a built-in receiver is unaffected';
