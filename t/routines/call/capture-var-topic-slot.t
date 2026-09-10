use v6;
use Test;

# `$<name>` is sugar for `$/<name>`, so it must read the SAME `$/` the `$/`
# variable itself reads. Inside a `method foo($/)`, `$/` is bound to the match
# parameter (a local slot). A nested regex operation (`.subst`, `~~`, `m//`)
# run in the method body writes the dynamic `$/` (env) to its own — possibly
# failed — match. That must NOT change what `$<name>` sees: the twigil has to
# keep reading the method's `$/` parameter, exactly as `$/<name>` does.
#
# Regression: `$<name>` read env `/` directly, so a failed `.subst` clobbered it
# to Nil and `$<absent>` became `Any` (an unmatched capture must stay `Nil`).
# This surfaced in the YAMLish `plain` action (`$<properties><tag>.ast` threw
# `No such method 'ast' for invocant of type 'Any'`).

plan 6;

grammar G {
    token TOP { <thing> }
    token thing { \d }
    class Actions {
        method thing($/) {
            # A subst that does NOT match sets the dynamic `$/` to a failed match.
            my $s = "42";
            $s .= subst(/<[\ \t]>+$/, '');
            # The absent capture must still be Nil (not Any), matching $/<absent>.
            is $<absent>.^name, 'Nil',
                '$<absent> stays Nil after a failed nested subst';
            is $/<absent>.^name, 'Nil',
                '$/<absent> stays Nil after a failed nested subst';
            is ($<absent> eqv $/<absent>), True,
                '$<name> is identical to $/<name>';
            # Chained access on the absent capture does not throw.
            lives-ok { my $x = $<absent><inner> }, 'chained access on absent capture lives';
            make 99;
        }
    }
}

my $m = G.parse("4", :actions(G::Actions));
ok $m.defined, 'parse succeeded';
is $m<thing>.ast, 99, 'action ran and made its value';
