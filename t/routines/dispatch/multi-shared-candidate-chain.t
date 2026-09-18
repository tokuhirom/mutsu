use Test;

plan 6;

# A multi dispatch frame no longer materializes its own copy of the candidate
# list: every frame is a view (start index + skipped winner fingerprint) over
# the shared, per-generation candidate list (#8727). These cases pin the
# behaviour that sharing must not disturb.

# 1. A straight callsame chain still walks every candidate in order.
multi sub walk(Int $x) { "i" ~ callsame() }
multi sub walk(Cool $x) { "c" ~ callsame() }
multi sub walk(Any $x) { "a" }
is walk(1), "ica", "callsame walks the whole candidate chain";

# 2. `lastcall` truncates only its OWN frame. With a shared candidate list a
# frame that mutated the list in place would truncate every other live frame
# over the same family too, including the recursive caller below it.
multi sub nest(Int $x) {
    if $x > 0 {
        my $inner = nest($x - 1);
        return "int($x)<$inner>+" ~ callsame();
    }
    lastcall;
    return "base:" ~ (callsame() // 'Nil');
}
multi sub nest(Any $x) { "any($x)" }
is nest(2), "int(2)<int(1)<base:Nil>+any(1)>+any(2)",
    "lastcall in a nested frame leaves the outer frames' chains intact";

# 3. After that nested run the family is untouched for a fresh dispatch.
is nest(0), "base:Nil", "a later dispatch of the same family still resolves";
is nest("x"), "any(x)", "and still reaches the wide candidate directly";

# 4. `nextcallee` advances the same shared chain: it CONSUMES the candidate it
# hands back, so a following callsame in the same frame has nowhere left to go.
multi sub pick(Int $x) {
    my $next = nextcallee();
    "i" ~ $next($x) ~ (callsame() // 'Nil');
}
multi sub pick(Any $x) { "a" }
is pick(1), "iaNil", "nextcallee consumes the candidate it returns";

# 5. A single-candidate multi is still a dispatcher with nowhere to defer to.
multi sub lone(Int $x) { "lone:" ~ (callsame() // 'Nil') }
is lone(1), "lone:Nil", "the only candidate's callsame is Nil, not an error";
