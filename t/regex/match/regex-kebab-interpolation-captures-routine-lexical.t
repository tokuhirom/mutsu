use Test;

# A regex literal written in a routine closes over a kebab-case lexical
# (`$full-path`) it interpolates, so matching it inside another routine (here
# Test's `like`) reads the regex's own lexical, not the matching frame's
# (#12478). Checked against rakudo.

plan 3;

sub check-line($line) {
    my $full-path = '/x/y';
    like "at /x/y line 9", /'at ' $full-path ' line ' $line/, 'kebab-case and plain lexicals';
}
check-line(9);

sub check-only {
    my $the-word = 'abc';
    like 'xabcx', /$the-word/, 'a lone kebab-case lexical';
}
check-only();

sub matcher($s, $re) { so $s ~~ $re }
sub build {
    my $some-name = 'needle';
    matcher('haystack needle', /$some-name/)
}
ok build(), 'matched in a plain routine';

done-testing;
