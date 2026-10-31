use Test;

# A private method call (`self!m($j)`) binds a Junction to a `Mu` parameter
# whole, as a public call does -- it is not run once per eigenstate (#15842,
# from Data::Dump::Tree's `self!get_address($junction)`).

plan 6;

class M {
    multi method hdr (Junction $j) { 'junction candidate' }
    multi method hdr ($a) { 'any candidate' }
}

class A {
    method !takes-mu(Mu $e) { $e.^name }
    method !takes-any($e) { $e.^name }
    method pub(Mu $e) { $e.^name }
    method mu-both(Mu $j) { (self!takes-mu($j), self.pub($j)) }
    method any-private(Mu $j) { self!takes-any($j) }
}

my $j = 1 | 2;
is-deeply A.new.mu-both($j), ('Junction', 'Junction'), 'private and public Mu parameters take the Junction whole';
my $r = A.new.any-private($j);
isa-ok $r, Junction, 'an untyped (Any) private parameter still threads';
is $r.raku, 'any("Int", "Int")', 'once per eigenstate';
is A.new.mu-both(5).join(','), 'Int,Int', 'a plain value is unchanged';

# A multi method with a `Junction` candidate gets the Junction whole (DDT's
# `multi method get_header (Junction $j)`); a plain value takes the other.
is M.new.hdr($j), 'junction candidate', 'a Junction candidate of a multi method takes the Junction';
is M.new.hdr(5), 'any candidate', 'a plain value takes the Any candidate';
