use Test;
plan 6;

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

my $angle = Q:to<--END-->;
first
second
--END--
is $angle, "first\nsecond\n", 'Q:to<...> heredoc';

my $indented = q:to/TERM/;
alpha
    TERM
is $indented, "alpha\n", 'q:to heredoc accepts indented terminator';

my $dedented = q:to/END/;
    red
    green
    END
is $dedented, "red\ngreen\n", 'q:to strips terminator indentation from content';
