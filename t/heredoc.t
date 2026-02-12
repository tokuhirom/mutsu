use Test;
plan 3;

my $text = q:to/END/;
Hello
World
END
is $text, "Hello\nWorld\n", 'basic q:to heredoc';

my $multi = q:to/STOP/;
line one
line two
line three
STOP
is $multi, "line one\nline two\nline three\n", 'multi-line heredoc';

my $empty = q:to/END/;
END
is $empty, "", 'empty heredoc';
