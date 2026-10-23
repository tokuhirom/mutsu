use Test;

# The text of a quoted string or heredoc carries the origin Rakudo gives it
# (ADR-10723 Stage 2): the range inside the quote's delimiters, escapes as
# written, and a heredoc's body lines. Ranges measured on rakudo 2026.09.

plan 7;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub text($quote) { my @c; $quote.visit-children(-> $c { @c.push: $c }); @c[0] }

my $src = "say \"abc\", 'x', Q\{a\nb\}, \"a\\tb\", q:to/E/;\nzz\nE\n";
my @args = $src.AST.statements[0].expression.args.args;

is range(text(@args[0])), '5..8', 'a double-quoted string';
is range(text(@args[1])), '12..13', 'a single-quoted string';
is range(text(@args[2])), '18..21', 'a Q-quoted string over two lines';
is range(text(@args[3])), '25..29', 'a string with an escape, as written';
is range(@args[4]), '32..39', 'a heredoc';
is range(text(@args[4])), '41..44', 'its body';

my @lines;
sub walk($node) {
    @lines.push: .source.original-line(.from) with $node.origin;
    $node.visit-children(&walk);
}
q:to/END/.AST.visit-children(&walk);
my $doc = q:to/TEXT/;
    first
    second
    TEXT
say $doc;
END
is @lines.sort.squish.join(','), '1,2,5',
    'a heredoc body reports the line it starts on';
