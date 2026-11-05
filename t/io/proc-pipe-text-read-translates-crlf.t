use Test;

# Prompt::Hidden t/01-plain.t: a child that prints "\r\n" is read back through
# `run(..., :out)` as "\n" -- a text-mode handle reads the CR-LF grapheme as LF,
# a pipe like a file.

plan 3;

my $exe = $*EXECUTABLE;

my $p = run $exe, '-e', 'print "x\r\ny"', :out;
is $p.out.slurp(:close), "x\ny", 'slurp of a pipe';

$p = run $exe, '-e', 'print "a\r\nb\r\nc\r\n"', :out;
is $p.out.lines.join('|'), 'a|b|c', 'lines of a pipe';

$p = run $exe, '-e', 'print "q\r\n"', :out, :bin;
is $p.out.slurp(:close, :bin).decode, "q\r\n", 'a binary handle keeps the bytes';

done-testing;
