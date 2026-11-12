use Test;

plan 5;

# A CATCH handler sees its own frame's variables as writable even when the
# exception came from a routine whose read-only parameter has the same name
# (the handler runs at the throw site, above that routine's frame).

class T { method unsupported(Str $reason) { die $reason } }
my $t = T.new;

sub f() {
    my $reason = '';
    try {
        $t.unsupported("boom");
        CATCH { default { $reason = .message } }
    }
    $reason
}
is f(), 'boom', 'a method parameter of the same name does not make it read-only';

sub g($n) {
    g($n - 1) if $n > 0;
    my $reason = '';
    try {
        $t.unsupported("r$n");
        CATCH { default { $reason = .message } }
    }
    $reason
}
is g(2), 'r2', 'in a recursive routine';

sub thrower(Str $reason) { die $reason }
sub h() {
    my $reason = '';
    try {
        thrower('sub');
        CATCH { default { $reason = .message } }
    }
    $reason
}
is h(), 'sub', 'a sub parameter of the same name too';

sub ro($x) {
    try {
        die 'x';
        CATCH { default { } }
    }
    $x = 1;
}
dies-ok { ro(5) }, 'the handler frame keeps its own read-only parameter';

sub k(Str $reason) {
    my $out;
    try {
        thrower('inner');
        CATCH { default { $out = .message; } }
    }
    "$out/$reason"
}
is k('outer'), 'inner/outer', 'and its own parameter value';
