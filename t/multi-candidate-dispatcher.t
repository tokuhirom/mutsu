use Test;

# `.dispatcher` on a multi candidate is the proto that dispatches it -- Raku
# generates one even when the source declares no `proto`, and `.candidates` on
# it lists the whole multi.
#
# mutsu answered the *candidate itself* (and, for a `Sub`-shaped value, an
# ADR-0070 `<composed-method:dispatcher>` stub), so `.dispatcher.candidates`
# reported a single candidate. That silently narrows the
# `Map.new('&name' => $candidate.dispatcher)` idiom a custom `sub EXPORT` uses
# to re-export a multi -- `NativeLibs` re-exports `&trait_mod:<is>` that way.
#
# Ground truth from rakudo 2026.07: a candidate's `.dispatcher` is a `Sub` with
# `.is_dispatcher` True and every candidate; a routine in no multi has no
# dispatcher at all (rakudo answers an NQPMu, which is falsy).
plan 8;

sub plain($x) { $x }
multi sub no-proto(Int $x) { 'int' }
multi sub no-proto(Str $s) { 'str' }
proto sub with-proto(|) {*}
multi sub with-proto(Int $x) { 'int' }
multi sub with-proto(Str $s) { 'str' }

nok &plain.dispatcher, 'a routine in no multi has no dispatcher';

for '&no-proto', '&with-proto' -> $name {
    my $cand = ::($name).candidates[0];
    my $disp = $cand.dispatcher;
    ok $disp, "$name candidate has a dispatcher";
    is $disp.candidates.elems, 2, "$name dispatcher lists every candidate";
    ok $disp.is_dispatcher, "$name dispatcher answers .is_dispatcher";
}

# The shape the `sub EXPORT` idiom depends on: pick a candidate by matching its
# signature, then re-export the dispatcher, and the multi is still whole.
my $native-ish = &no-proto.candidates.first({ .signature ~~ :(Str) });
is $native-ish.dispatcher.candidates.elems, 2,
    'a dispatcher reached through .candidates.first still carries the whole multi';
