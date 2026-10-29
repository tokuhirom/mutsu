use Test;

plan 7;

# A parametric role whose parent is its own type parameter (`role P[::H] is H`)
# keeps that parent when it is composed through another role
# (`role Q[::H] does P[H]`), so a user class doing Q is a subclass of the
# bound HOW class and mints types whose HOW is an instance of it.
my role P[::H] is H { has $.e is rw; method ANN { 7 } }
my class C does P[Metamodel::ClassHOW] { }
is C.^mro.map(*.^name).join(' '), 'C Perl6::Metamodel::ClassHOW Any Mu', 'direct composition';

my role Q[::H] does P[H] { method new_type(|c) is raw { callwith |c } }
my class D does Q[Metamodel::ClassHOW] { }
is D.^mro.map(*.^name).join(' '), 'D Perl6::Metamodel::ClassHOW Any Mu', 'composition through a role';
my $u := D.new_type(:name<y>);
is $u.HOW.^name, 'D', 'the minted type\'s HOW is an instance of the user class';
is $u.HOW.ANN, 7, 'with the role\'s methods';

my role AnnHOW[Positional ::T, ::H] is H {
    has @!annotations;
    submethod BUILD { @!annotations := T.new }
    method ANN is raw { @!annotations }
}
my role RecHOW[::F] does AnnHOW[Array, Metamodel::ClassHOW] {
    method new_type(::?ROLE:_: :$name, *%rest) is raw {
        my $obj := callwith :name($name), |%rest;
        $obj.HOW.ANN = 1, 2, 3;
        $obj
    }
}
my class Recorder does RecHOW[List] { }
my $t := Recorder.new_type(:name<Named>);
is $t.^name, 'Named', 'the minted type is named';
is $t.HOW.ANN.raku, '[1, 2, 3]', 'annotations stored on its HOW';
is Recorder.new.ANN.raku, '[]', 'the HOW class constructs with the role\'s BUILD';
