use Test;
plan 9;

# A `rule`/`token` may declare a *dynamic* variable inline
# (`:my %*PLAYED = ();`). It must not be mangled by the four whitespace/
# interpolation passes a rule body goes through, and it lives in the dynamic
# scope of the whole parse so action methods and `<?{ … }>` code assertions run
# during the match share it. (roast/integration/advent2013-day18.t)

# --- `:my %*VAR` survives compilation (no "Null regex not allowed") ---
{
    my grammar G {
        rule TOP { :my %*SEEN = (); <word>+ % \s }
        token word { \w+ }
    }
    ok G.parse("a b c").defined, ':my %*VAR in a rule body compiles and matches';
}

# --- sigspace: whitespace before a closing `]`/`)` is significant ---
{
    my grammar H {
        token w { \w }
        rule r { [ <w> ]**3 }
    }
    ok H.parse("a b c", :rule<r>).defined, 'trailing space in [ <w> ] is a required <.ws>';
    nok H.parse("abc", :rule<r>).defined, 'no space does not match the sigspace group';
}

# --- action methods share a rule-declared dynamic hash ---
{
    my grammar G2 {
        rule TOP { :my %*SEEN = (); <word>+ % \s }
        token word { \w+ }
    }
    my class Act {
        has @.dups = ();
        method word($/) { @.dups.push(~$/) if %*SEEN{~$/}++; }
    }
    my $a = Act.new;
    G2.parse("a b a c b", :actions($a));
    is-deeply $a.dups, ["a", "b"], 'action methods accumulate into %*SEEN';
}

# --- `<?{ … }>` code assertion: `$/.Str` is the matched-so-far text ---
{
    my grammar G3 {
        rule TOP { <word> }
        token word { \w+ <?{ $/.Str eq 'abc' }> }
    }
    ok G3.parse("abc", :rule<word>).defined, '$/.Str in <?{ }> is the matched text';
}

# --- code assertion drives dedup via a shared dynamic hash ---
{
    my grammar G4 {
        rule TOP { :my %*S = (); <word>+ % \s }
        token word { \w+ <?{ ! %*S{$/.Str}++ }> }
    }
    ok G4.parse("a b c").defined, 'no duplicates parses';
    nok G4.parse("a b a").defined, 'a duplicate fails the parse via the shared %*S';
}

# --- inside a code assertion, `$/` is a Match: `$/[*-1]` indexes captures ---
# (`*-1` resolves against the Match's positional-capture list length).
{
    ok ("1"  ~~ m/ (\d) <?{ $/[*-1] < 5 }> /), '$/[*-1] in <?{ }> indexes the last capture';
    ok ("5" !~~ m/ (\d) <?{ $/[*-1] < 5 }> /), '$/[*-1] comparison fails the assertion when false';
}
