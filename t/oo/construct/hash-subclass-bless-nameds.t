use Test;

plan 6;

# #13087: `bless`'s named arguments only initialize attributes and feed
# BUILD/TWEAK; on an `is Hash` subclass they never become hash entries.
# (Reduced from PDF's `PDF::COS::Dict.new`, `self.bless(:$dict, |c)`.)
class D is Hash {
    submethod TWEAK(:$x) { self<k> = $x }
    method new(|c) { self.bless(:x(1), |c) }
}
my $d = D.new;
is $d.keys.sort, ('k',), 'a bless named argument reaches TWEAK, not the hash';
is $d<k>, 1, 'TWEAK stored its entry';

class G is Hash { }
is G.bless(a => 1).keys.elems, 0, 'a bare bless stores no entries';
is G.new(a => 1, b => 2).keys.sort, <a b>, 'Hash.new on a subclass still stores its pairs';

class H is Hash {
    has $.attr;
    method new(*%h) { self.bless(attr => %h<attr>) }
}
my $h = H.new(attr => 5);
is $h.attr, 5, 'a bless named argument initializes the attribute';
is $h.keys.elems, 0, 'and is not a hash entry';
