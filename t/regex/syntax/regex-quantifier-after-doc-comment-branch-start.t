use Test;
plan 13;

# Right after a branch opener (`|`, `||`, a group's `[` / `(`), a quantifier that
# follows a bracketed comment quantifies nothing and rakudo drops it instead of
# refusing the regex. ANTLR4::Grammar emits `#|{action}?` there for an ANTLR
# semantic predicate. With no comment, after a line comment, or outside a branch
# it is still `Quantifier quantifies nothing`.

sub verdicts($g) { <b bb ab>.map({ $_ ~ '=' ~ ($g.parse($_, :rule<t>) ?? 'm' !! 'n') }).join(' ') }
my $nl = "\n";
my $n = 0;
sub grammar-matches(Str $body) {
    my $name = 'QDC' ~ ++$n;
    my $g = EVAL "grammar $name \{ token t \{ $body \} \}; $name";
    verdicts($g);
}
sub compiles(Str $body) {
    my $name = 'QDC' ~ ++$n;
    so try { EVAL "grammar $name \{ token t \{ $body \} \}"; True }
}

is grammar-matches("|| #|\{x}?{$nl} 'b'"), 'b=m bb=n ab=n', '|| #|{x}? b: the ? is dropped';
is grammar-matches("|| #`\{x}?{$nl} 'b'"), 'b=m bb=n ab=n', '|| #`{x}? b';
is grammar-matches("| #|\{x}?{$nl} 'b'"), 'b=m bb=n ab=n', '| #|{x}? b';
is grammar-matches("[ #|\{x}?{$nl} 'b' ]"), 'b=m bb=n ab=n', 'a group opener';
is grammar-matches("|| #|\{x}*{$nl} 'b'"), 'b=m bb=n ab=n', '* is dropped too (no repetition)';
is grammar-matches("|| #|\{x}+{$nl} 'b'"), 'b=m bb=n ab=n', '+ is dropped too';
is grammar-matches("|| #|\{x}??{$nl} 'b'"), 'b=m bb=n ab=n', '?? is dropped too';
is grammar-matches("|| #|\{x} ?{$nl} 'b'"), 'b=m bb=n ab=n', 'whitespace between the comment and the quantifier';
is grammar-matches("|| #|\{x}?{$nl} #|\{y}?{$nl} 'b'"), 'b=m bb=n ab=n', 'two comments in a row';

# An atom before the comment: the quantifier is that atom's, as before.
is grammar-matches("'a' #|\{x}?{$nl} 'b'"), 'b=m bb=n ab=m', "'a' #|\{x}? b quantifies 'a'";

# Still refused.
nok compiles("||   ?{$nl} 'b'"), 'a bare ? after || (no comment) is refused';
nok compiles("|| # line{$nl}?{$nl} 'b'"), 'a line comment does not count';
nok compiles("#|\{x}?{$nl} 'b'"), 'outside a branch (the regex start) it is refused';
