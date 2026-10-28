use Test;

# Found via Math::Libgsl::Raw::MovingWindow (#15226): `Nil is Cool`, so a Cool
# string method called on Nil coerces it to "" (with a warning) rather than
# answering Nil.

plan 19;

my @w;
my sub quiet(&code) {
    CONTROL { when CX::Warn { @w.push(.message); .resume } }
    code();
}

for <chars chop chomp uc lc tc flip codes lines words comb> -> $m {
    @w = ();
    my $r = quiet({ Nil."$m"() });
    is-deeply $r, "", "Nil.$m returns the empty string";
}

@w = ();
is-deeply quiet({ Nil.comb(/\S+/) }), "", "Nil.comb with a matcher";
is @w.elems, 1, "one warning";
is @w[0], "Use of Nil.comb coerced to empty string", "warning names the method";

is-deeply quiet({ Nil.substr(0) }), "", "Nil.substr(0)";
is-deeply quiet({ Nil.contains("a") }), "", "Nil.contains";
is-deeply quiet({ Nil.index("a") }), "", "Nil.index";
is-deeply quiet({ Nil.abs }), 0, "Nil.abs stays numeric zero";
is-deeply quiet({ Nil.ITotallyMadeThisUp }), Nil, "unknown method still absorbed";
