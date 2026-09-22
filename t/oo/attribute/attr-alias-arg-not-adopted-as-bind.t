use Test;

plan 6;

# A named parameter written in the alias form (`:d(:$directed)`) declares the
# lexical `$directed`, but the signature's own parameter name is the external
# key `d`. The method-exit attribute reconcile used to consult only the flat
# parameter-name list, so such a parameter did not look like one the frame
# owned; once it had been boxed into a container (by ending up inside a list),
# it was adopted as a `:=` binding for the same-named attribute and written
# into the RECEIVER's attribute cell. A method that only reads `self` and
# returns a brand-new object therefore mutated the object it was cloning from
# (#9007, found via the Graph distribution's `directed-graph`).

class Graphish {
    has Bool:D $.directed = False;
    submethod BUILD(Bool:D :directed-edges(:$!directed) = False) { }
    multi method new(Bool:D :d(:directed-edges(:$directed)) = False) {
        self.bless(:$directed);
    }
    method clone(:d(:$directed) is copy = Whatever) {
        $directed = $!directed if $directed.isa(Whatever);
        given ($directed, $!directed) {
            when $_ eqv (True, False) { }
        }
        return Graphish.new(:$directed);
    }
    method directed-graph() {
        return $!directed ?? self.clone !! self.clone(:directed);
    }
}

my $g = Graphish.new;
is $g.directed, False, 'receiver starts undirected';
my $d = $g.directed-graph;
is $d.directed, True, 'the returned clone is directed';
is $g.directed, False, 'the receiver is NOT mutated by the clone';

# The same shape without the alias spelling always worked; keep it pinned so a
# future narrowing of the guard cannot regress it either.
class Plain {
    has Bool:D $.flag = False;
    submethod BUILD(Bool:D :$!flag = False) { }
    method copy(:$flag is copy = Whatever) {
        $flag = $!flag if $flag.isa(Whatever);
        my $pair = ($flag, $!flag);
        return Plain.bless(:$flag);
    }
}
my $p = Plain.new;
is $p.copy(:flag).flag, True, 'plain-spelling copy is flagged';
is $p.flag, False, 'plain-spelling receiver is NOT mutated';

# A destructuring sub-signature binds lexicals the flat name list does not
# carry either, and those names must not be adopted as attribute bindings.
class Boxed {
    has $.x = 'attr';
    method take([$x, $y]) {
        my $pair = ($x, $y);
        return "$x/$y";
    }
}
my $b = Boxed.new;
$b.take([1, 2]);
is $b.x, 'attr', 'a destructured sub-signature name is not adopted as an attribute bind';
