use v6;
use Test;
use nqp;

# Source: DirHandle / P5opendir. Binding a native `str` array into a
# same-typed attribute must keep it a native array (nqp::atpos_s accepts only
# a VMArray), for the bound source as well as the attribute.

plan 5;

class C {
    has str @.items handles <elems>;
    has int $.index;
    method set {
        my str @x;
        nqp::push_s(@x, "p");
        nqp::push_s(@x, "q");
        @!items := @x;
        is nqp::atpos_s(@x, 0), "p", 'bound source still a native array';
        self
    }
    method next() {
        $!index < nqp::elems(@!items) ?? nqp::atpos_s(@!items, $!index++) !! Nil
    }
}

my $c = C.new.set;
is $c.next, "p", 'atpos_s on the bound attribute';
is $c.next, "q", 'second element';
is $c.next.raku, "Nil", 'past the end';
is $c.elems, 2, 'handles <elems>';
