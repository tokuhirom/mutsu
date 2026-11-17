use experimental :rakuast;
use Test;

# A call of a `multi` declared in a nested block resolves to the block's
# dispatcher (its `proto`, or the one its first candidate implies, #17836).
# The block has not run at CHECK time, so the dispatcher is not installed
# yet; its `compile-time-value` is still the dispatcher over the block's
# candidates, as in rakudo 2026.09 (#17826).

plan 11;

my %seen;
CHECK {
    $*CU.visit: -> $n {
        if $n ~~ RakuAST::Call::Name && $n.name.canonicalize eq 'bz' | 'pz' | 'cnt' {
            my $d = $n.resolution.compile-time-value;
            %seen{$n.name.canonicalize} //= %(
                multiness  => $n.resolution.multiness,
                dispatcher => $d.is_dispatcher,
                signatures => $d.candidates.map(*.signature.raku).List,
            );
        }
        True
    }
}

my @ran;
{
    multi bz(Int $x) { "i" }
    multi bz(Str $x) { "s" }
    @ran.push: bz(1), bz("a");
}
{
    proto pz($) {*}
    multi pz(Int $x) { "pi" }
    @ran.push: pz(1);
}
{
    multi cnt($x) { $x ?? cnt($x - 1) !! "done" }
    @ran.push: cnt(2);
}

is %seen<bz><multiness>, 'proto', 'a block-local multi call resolves to the implied proto';
ok %seen<bz><dispatcher>, 'its value is a dispatcher';
is-deeply %seen<bz><signatures>, (':(Int $x)', ':(Str $x)'),
    "holding the block's candidates in declaration order";
is %seen<pz><multiness>, 'proto', "a block's written proto is the resolution";
ok %seen<pz><dispatcher>, 'its value is a dispatcher';
is-deeply %seen<pz><signatures>, (':(Int $x)',), 'with its candidate';
ok %seen<cnt><dispatcher>, "a candidate's call of its own name sees the dispatcher";
is %seen<cnt><signatures>.elems, 1, 'with the one candidate';
is @ran.join(','), 'i,s,pi,done', 'the routines still dispatch';

my $hand = RakuAST::Sub.new(name => RakuAST::Name.from-identifier('h'), :multiness<proto>);
nok $hand.compile-time-value.defined && $hand.compile-time-value.is_dispatcher
    && $hand.compile-time-value.candidates.elems,
    'a hand-built proto carries no block candidates';
ok %seen<bz>:exists, 'the probe saw the calls';
