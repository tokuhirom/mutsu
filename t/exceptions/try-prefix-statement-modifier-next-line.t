use Test;

# `try EXPR` is a statement PREFIX: the statement ends where the expression
# ends, so a statement modifier may continue on the following line, exactly as
# it may after any other expression. Only the BLOCK form `try { ... }` ends its
# statement at the closing brace, which is what lets the next line's `if` start
# a fresh statement.
#
# mutsu decided this on the `Expr::Try` variant alone, so the prefix form took
# the block path too: the modifier parser was handed the text before the
# newline, declined the modifier, and the bare `if` was then parsed as an `if`
# statement -- reported as `Missing block`.
#
# From `Selkie::App::Internal::ErrorLog`, which both `Selkie` and
# `App::Moneymoor` fail to load on:
#
#     try windows-close-handle($log-windows-handle)
#         if $log-windows-handle.defined
#             && $log-windows-handle.Int != INVALID-HANDLE-VALUE;
#
# https://github.com/tokuhirom/mutsu/issues/7993

plan 8;

my @ran;
sub note-it($x) { @ran.push($x); $x }

my $h = 5;

try note-it('same-line') if $h.defined;
is @ran, ['same-line'], 'try EXPR with a same-line `if` modifier';

@ran = ();
try note-it('next-line')
    if $h.defined;
is @ran, ['next-line'], 'try EXPR with the `if` modifier on the next line';

@ran = ();
try note-it('continued')
    if $h.defined
        && $h != -1
        && $h != 0;
is @ran, ['continued'], 'the modifier condition may itself span further lines';

@ran = ();
try note-it('not-run')
    if $h > 100;
is @ran, [], 'a false next-line modifier condition suppresses the call';

@ran = ();
try note-it('unless')
    unless $h > 100;
is @ran, ['unless'], '`unless` works the same way';

# `try` still swallows the exception when the modifier fires.
@ran = ();
lives-ok {
    try die('boom')
        if $h.defined;
}, 'try still catches when the next-line modifier fires';

# Guard: the BLOCK form must keep terminating its statement at the brace, so a
# following `if` is a new statement and not a modifier.
my $block-if-ran = 0;
try {
    die 'ignored';
}
if $h { $block-if-ran = 1 }
is $block-if-ran, 1, '`try { ... }` followed by a new-line `if` STATEMENT still works';

# Same guard for `gather`, which shares the code path.
my @g = gather {
    take 1;
    take 2;
}
if $h { @g.push(3) }
is @g, [1, 2, 3], '`gather { ... }` followed by a new-line `if` STATEMENT still works';
