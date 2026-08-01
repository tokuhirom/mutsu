use v6;
use Test;

# A heredoc whose terminator contains multi-byte characters must not break the
# postfix parser's line-ending-block-rule bookkeeping: the heredoc reader
# resumes AFTER the terminator line, so the remainder is not a suffix of the
# expression fragment being parsed, and a byte-length subtraction sliced
# mid-char and panicked (roast/S02-literals/quoting.t line 357).

plan 3;

my $t = q:to /结束/;
Hello, World
结束
is $t, "Hello, World\n", 'q:to with a multi-byte terminator parses and yields the body';

my $u = q:to /END/;
plain
END
is $u, "plain\n", 'ASCII terminator still works';

# The line-ending-block rule itself still holds after a heredoc.
my $x = do given 42 { $_ }
is $x, 42, 'given block after heredoc parses';
