use Test;
use nqp;

# A call of a `multi` routine resolves to its dispatcher, as in rakudo: the
# resolution is a `RakuAST::Sub` whose `multiness` is `proto` (the implied
# proto, or the explicit one), and its `compile-time-value` is the dispatcher
# with every candidate, each the very object its own declaration answers.
# static's analyzer dispatches over `.candidates` of that value (t/03, t/09).

plan 7;

my %seen;
multi sub f(Int $x) { 1 }
multi sub f(Str $x) { "s" }
f(1);
proto sub g($) {*}
multi sub g(Int $x) { 2 }
g(1);

CHECK {
    my @decls;
    $*CU.visit: -> $n {
        if $n.^name eq 'RakuAST::Sub' && $n.multiness eq 'multi'
            && $n.name.canonicalize eq 'f' {
            @decls.push: nqp::objectid($n.compile-time-value);
        }
        if $n.^name eq 'RakuAST::Call::Name' {
            my $r = $n.resolution;
            my $d = $r.compile-time-value;
            %seen{$n.name.canonicalize} = %(
                multiness  => $r.multiness,
                candidates => $d.candidates.elems,
                dispatcher => $d.is_dispatcher,
                ids        => $d.candidates.map({ nqp::objectid($_) }).List,
            );
        }
        True
    }
    %seen<decl-ids> = @decls.List;
}

is %seen<f><multiness>, 'proto', 'a multi call resolves to the implied proto';
is %seen<f><candidates>, 2, 'its value holds every candidate';
ok %seen<f><dispatcher>, 'and is the dispatcher';
is-deeply %seen<f><ids>, %seen<decl-ids>,
    'each candidate is the object its declaration answers';
is %seen<g><multiness>, 'proto', 'an explicit proto is the resolution';
is %seen<g><candidates>, 1, 'with its candidate';
is (f(1), f("x"), g(1)).join(','), '1,s,2', 'the routines still dispatch';
