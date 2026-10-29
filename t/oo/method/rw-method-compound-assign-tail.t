use Test;

plan 8;

# Reduced from Red's MetamodelX::Red::Model: `method table(Mu \type) is rw {
# $!table //= ... }` (t/23-metamodel-model.rakutest). A compound assignment as
# the tail of an `is rw` method hands back the assigned variable's container.

class H {
    has $!t;
    has $.u is rw;
    method t(Mu \x) is rw { $!t //= "bla" }
    method n() is rw { $!t ||= "five" }
}

my $h = H.new;
is $h.t(1), "bla", 'the initialising value is returned';
$h.t(1) = "x";
is $h.t(1), "x", 'assigning through the rw method writes the attribute';
is $h.n, "x", 'a second rw method sees the same attribute';
$h.n = "y";
is $h.t(1), "y", '||= tail also exposes the container';

class MyHOW is Metamodel::ClassHOW {
    has $!table;
    method table(Mu \type) is rw { $!table //= "bla" }
}
my \Bla = MyHOW.new_type(:name<Bla>);
Bla.^compose;
is Bla.^table, "bla", 'metamethod returns the default';
Bla.^table = "not_bla";
is Bla.^table, "not_bla", 'metamethod assignment on a type object';

sub counter() is rw { state $c //= 10 }
is counter(), 10, 'sub with a compound-assign tail';
counter() = 11;
is counter(), 11, 'sub tail container is writable';
