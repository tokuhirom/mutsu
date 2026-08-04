use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# A parse error whose column falls after (or inside) a multi-byte UTF-8
# character must not crash the compiler. Previously format_parse_error() sliced
# the source line at a character column used as a byte offset, panicking with
# "byte index N is not a char boundary".

plan 3;

# Multi-byte characters (é, î) before the offending token.
is_run(
    'say "aéî"" if False;',
    {
        status => sub { 0 != $^a },
        err    => rx/'SORRY'/,
    },
    'parse error after multi-byte chars reports SORRY, not a panic',
);

# The error output must not contain a Rust panic message.
is_run(
    'my $x = "ключ"" ;',
    {
        status => sub { 0 != $^a },
        err    => sub { $^e !~~ /:i 'panic' | 'char boundary'/ },
    },
    'parse error after Cyrillic chars does not panic',
);

# Sanity: the same shape with ASCII-only source still reports SORRY, so the two
# cases above are testing the encoding and not the diagnosis.
is_run(
    'my $x = "abc"" ;',
    {
        status => sub { 0 != $^a },
        err    => rx/'SORRY'/,
    },
    'ASCII parse error still reports SORRY',
);
