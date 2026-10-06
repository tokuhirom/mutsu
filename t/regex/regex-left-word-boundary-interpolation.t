use Test;

# Found via the Inline::BASIC distribution: `s:g/<<$name>>/$value/`.
plan 6;

my $n = "A";
is ("A + B" ~~ /<<$n/).Str, "A", '<<$n matches at a word start';
is ("A + B" ~~ /<< $n/).Str, "A", '<< $n with whitespace';
is ("XA + B" ~~ /<<$n/).Bool, False, '<<$n respects the boundary';
my $e = "A + B";
$e ~~ s:g/<<$n>>/5/;
is $e, "5 + B", 's:g/<<$n>>/ substitutes';
my $e2 = "AA + A";
$e2 ~~ s:g/<<$n>>/5/;
is $e2, "AA + 5", 'only whole words are replaced';
is ("A + B" ~~ /«$n»/).Str, "A", '«$n» still works';
