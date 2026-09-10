use Test;

# In a substitution `s/pattern/replacement/`, a trailing `$` in the pattern is
# the end-of-string anchor and the following `/` is the mandatory separator.
# mutsu's regex scanner mis-applied a match-regex heuristic that reads `$/` as
# the match variable when the char after the delimiter is `.`/`[`/`<`, swallowing
# the separator — so `s/foo$/.bar/` failed to parse. Found via the
# real-distribution compat sweep (PDF::Combiner, docs/dist-compat-sweep.md),
# which does `$outfile ~~ s/'.pdf'$/.{$Zip}dpi.pdf/`.

plan 9;

{ my $s = "x.pdf";  $s ~~ s/pdf$/.abc/;      is $s, "x..abc",  'anchor + replacement starting with `.`'; }
{ my $s = "x.pdf";  $s ~~ s/pdf$/./;         is $s, "x..",     'anchor + replacement of a single `.`'; }
{ my $s = "a[b";    $s ~~ s/b$/[z]/;         is $s, "a[[z]",   'anchor + replacement starting with `[`'; }
{ my $s = "a b";    $s ~~ s/b$/<z>/;         is $s, "a <z>",   'anchor + replacement starting with `<`'; }
{ my $Z = 150; my $s = "x.pdf"; $s ~~ s/'.pdf'$/.{$Z}dpi.pdf/; is $s, "x.150dpi.pdf", 'the PDF::Combiner idiom'; }

# Non-destructive S/// and adverbial forms
{ my $s = "foobar"; my $r = S/bar$/.X/ given $s; is $r, "foo.X", 'S/// non-destructive with anchor + `.`'; }
{ my $s = "aXaX";   $s ~~ s:g/X$/.Y/;        is $s, "aXa.Y",   ':g still anchors only the final match'; }

# The `$/` match variable inside a *match* regex is unaffected
{ "abc" ~~ /b/; is $/.Str, "b", 'match regex still exposes $/'; }
# A plain trailing-anchor substitution with a wordy replacement still works
{ my $s = "cat"; $s ~~ s/t$/s/; is $s, "cas", 'anchor + wordy replacement unchanged'; }
