use Test;

plan 1;

# The Cro shape: a named-capture alias whose char-class content excludes a
# literal `"`, with `"` also appearing inside adjacent single-quoted
# literals. None of this is a double-quoted interpolation region, so it
# must not trip the "dangerous regex interpolation" security check — the
# split-on-`"` parity heuristic previously misfired here because it did not
# track single-quote / character-class state.
#
# The security check itself (a genuine sigil inside double quotes must
# still be rejected, unbalanced braces, dynamic lookups, ...) is pinned by
# the whitelisted roast/S05-interpolation/regex-in-variable.t and is
# unaffected by this fix — not re-pinned here.
my $p = Q/'boundary="' $<b>=[<-["]>+] '"'/;
my $s = 'Content-type: multipart/form-data; boundary="abc123"';
ok $s ~~ /<$p>/,
    'quoted-literal " and char-class " do not misfire the double-quote heuristic';

done-testing;
