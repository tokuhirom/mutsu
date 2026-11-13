use Test;

# The callable `nextcallee` returns keeps the rest of the method's deferral
# chain: called later from a closure another method runs, it calls the next
# candidate, and a `callsame` in that candidate reaches the one after it
# (Red's ScopedDriver, #17501).

plan 3;

class G { method protect(&op) { op() } }
class D {
    method prepare($x) { "D.prepare($x)" }
    method execute($x) { "exec:" ~ self.prepare($x) }
}
my role Scoped {
    has G $.g;
    method prepare(|c) {
        my &next = nextcallee;
        $!g.protect: { "S[" ~ next(self, |c) ~ "]" }
    }
    method execute(|c) {
        my &next = nextcallee;
        $!g.protect: { next(self, |c) }
    }
}
my $d = D.new;
$d does role { method prepare(|args) { "R(" ~ callsame() ~ ")" } };
is $d.execute(1), 'exec:R(D.prepare(1))', 'callsame without the role';
my $s = ($d but Scoped).clone(:g(G.new));
is $s.execute(2), 'exec:S[R(D.prepare(2))]', 'nextcallee called from a closure';
is $s.prepare(3), 'S[R(D.prepare(3))]', 'and its candidate callsame continues the chain';
