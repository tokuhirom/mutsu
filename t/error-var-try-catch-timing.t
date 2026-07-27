use Test;

plan 15;

# `$!` is written when a `try` *completes*, not while a CATCH block runs.
# Inside CATCH the exception is the topic (`$_`); that block's own `$!` is Nil.

is $!.^name, 'Nil', 'initial $! is Nil';

sub fresh() { $!.^name }
is fresh(), 'Nil', 'a routine starts with a fresh Nil $!';

# --- what a completed try leaves behind ---------------------------------

sub ok-try() { try { 1 }; $!.^name }
is ok-try(), 'Any', 'a try that completes without an error leaves Any';

sub ok-after-failing() { try { die "boom" }; try { 1 }; $!.^name }
is ok-after-failing(), 'Any', 'a successful try clears an earlier exception to Any';

sub failing-try() { try { die "boom" }; $!.^name }
is failing-try(), 'X::AdHoc', 'an untrapped die in a try leaves the exception';

sub handled-try() { try { die "boom"; CATCH { default { } } }; $!.^name }
is handled-try(), 'Nil', 'a handled CATCH restores the pre-try $!';

sub handled-bare-block() { { die "boom"; CATCH { default { } } }; $!.^name }
is handled-bare-block(), 'Nil', 'a handled CATCH in a bare block restores $! too';

sub handled-keeps-prior() {
    try { die "first" };
    { die "second"; CATCH { default { } } };
    $!.message;
}
is handled-keeps-prior(), 'first', 'the restored $! is the prior exception, not the handled one';

# --- inside the CATCH block ---------------------------------------------

sub bang-in-try-catch() {
    my $seen;
    try { die "x"; CATCH { default { $seen = $!.^name } } };
    $seen;
}
is bang-in-try-catch(), 'Nil', '$! is Nil inside a CATCH block in a try';

sub bang-in-block-catch() {
    my $seen;
    { die "x"; CATCH { default { $seen = $!.^name } } };
    $seen;
}
is bang-in-block-catch(), 'Nil', '$! is Nil inside a CATCH block in a bare block';

sub topic-still-the-exception() {
    my $seen;
    try { die "x"; CATCH { default { $seen = .message } } };
    $seen;
}
is topic-still-the-exception(), 'x', 'the CATCH topic is still the exception';

sub bang-in-catch-after-earlier-error() {
    try { die "earlier" };
    my $seen;
    try { die "x"; CATCH { default { $seen = $!.^name } } };
    $seen;
}
is bang-in-catch-after-earlier-error(), 'Nil',
    'the CATCH block $! is Nil even when the scope already held an exception';

# --- nesting and rethrow -------------------------------------------------

sub nested-seen() {
    my $seen;
    try {
        try { die "inner" };
        $seen = $!.message;
        die "outer";
        CATCH { default { } }
    }
    $seen;
}
is nested-seen(), 'inner', 'an inner try publishes $! to the enclosing try body';

sub rethrown() {
    my $outer;
    try {
        try {
            die "boom";
            CATCH { when X::IO { } }   # no match -> rethrown
        }
        CATCH { default { $outer = .message } }
    }
    $outer;
}
is rethrown(), 'boom', 'an unmatched CATCH rethrows to the enclosing handler';

dies-ok { try { die "boom"; CATCH { when X::IO { } } } },
    'an unmatched CATCH rethrows out of its own try';
