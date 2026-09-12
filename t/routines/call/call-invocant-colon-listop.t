use Test;

# The listop invocant colon (`foo $x: @args`) dispatches a method on the
# first argument instead of calling the listop as a plain function:
# `foo $x: @args` means `$x.foo(@args)`. tokuhirom/mutsu#8141.

plan 7;

is do { say "hello": }, True, 'say EXPR: dispatches .say, which returns True';

my @a = 3, 1, 2;
is-deeply (sort @a:), (1, 2, 3), 'sort @a: dispatches Array.sort';

sub tail-return() { return "r": }
is tail-return(), 'r', 'return EXPR: as a sub tail dispatches .return';

sub mid-return() { return "r":; 'unreachable' }
is mid-return(), 'r', 'return EXPR: mid-body dispatches .return';

throws-like { my sub w() { warn "w": }; w() },
    X::Method::NotFound,
    'warn EXPR: as a sub tail has no .warn method on Str';

throws-like { my sub d() { die "boom": }; d() },
    X::Method::NotFound,
    'die EXPR: as a sub tail has no .die method on Str';

throws-like { die "boom":; 'unreachable' },
    X::Method::NotFound,
    'die EXPR: as a bare statement has no .die method on Str';
