use Test;

plan 4;

# A `=finish` line inside a heredoc body is text, not the end of the program
# (static's t/14-cli.rakutest writes a script holding one).
my $text = q:to/END/;
before
=finish
after
END
is $text, "before\n=finish\nafter\n", 'a heredoc keeps its =finish line';

my $two = qq:to/ONE/ ~ q:to/TWO/;
=finish
ONE
=finish
TWO
is $two, "=finish\n=finish\n", 'two heredocs on one line';

is 'still running', 'still running', 'the code after the heredocs runs';
is $=finish.trim, 'data after the real marker', 'the real =finish section is the data';

=finish
data after the real marker
