use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 17;

# A constant replacement (`s:g/\d+/#/`) comes back from the RakuAST round trip
# as the text itself, as the parser built it; only a replacement that is an
# expression (`$0`, a block, an interpolation) is evaluated once per match with
# `$/` bound (#15157). The behaviour is the same either way.

sub via-ast(Str $code) { EVAL($code.AST) }
sub on-sample(&f) { f("a1b22c333") }

my $t = 'a1b22c333';
is on-sample(sub ($c is copy) { $c ~~ s:g/\d+/#/; $c }), 'a#b#c#', 'constant replacement, :g';
is on-sample(sub ($c is copy) { $c ~~ s/\d+/-/; $c }), 'a-b22c333', 'constant replacement, first only';
is $t.subst(/\d+/, '#', :g), 'a#b#c#', '.subst with a constant';
is (S:g/\d+/<>/ given $t), 'a<>b<>c<>', 'S///: non-destructive, constant';
is on-sample(sub ($c is copy) { $c ~~ s:g/\d+//; $c }), 'abc', 'empty replacement';
is ('x y', -> $c is copy { $c ~~ s/' '/ and /; $c })[1].('x y'), 'x and y', 'replacement with spaces';

is via-ast(Q[my $c = 'a1b22'; $c ~~ s:g/\d+/+/; $c]), 'a+b+', 'the tree: constant, :g';
is via-ast(Q[my $c = 'a1b22'; $c ~~ s:g/(\d+)/<$0>/; $c]), 'a<1>b<22>', 'the tree: interpolating $0';
is via-ast(Q[my $c = 'a1b22'; $c ~~ s:g/\d+/{ $/.chars }/; $c]), 'a1b2', 'the tree: a block uses $/';
is via-ast(Q[my $c = 'a1'; $c ~~ s/\d+/\$/; $c]), 'a$', 'the tree: an escaped sigil is literal';
is via-ast(Q[my $c = 'a1'; $c ~~ s/\d+/\\/; $c]), 'a\\', 'the tree: an escaped backslash';
is via-ast(Q[my $c = 'abc'; $c ~~ s/b/[x]/; $c]), 'a[x]c', 'the tree: brackets are literal';
is via-ast(Q[my $c = 'a1'; $c ~~ s/\d+/&/; $c]), 'a&', 'the tree: a bare ampersand';
is via-ast(Q[my $n = 5; my $c = 'a1'; $c ~~ s/\d+/$n/; $c]), 'a5', 'the tree: a variable';

# The lowered shape: a constant keeps its text and carries no thunk; an
# expression carries the thunk (mutsu's own `--dump-ast`).
if $*EXECUTABLE.basename ~~ /mutsu/ {
    sub shape(Str $code) {
        my $p = run $*EXECUTABLE, '--dump-ast', '-e', $code, :out, :err;
        $p.out.slurp(:close)
    }
    my $const = shape('my $c = "a1"; $c ~~ s:g/\d+/X/');
    like $const, /'replacement: "X"'/, 'a constant replacement keeps its text';
    like $const, /'replacement_thunk: None'/, 'and carries no thunk';
    my $expr = shape('my $c = "a1"; $c ~~ s:g/(\d+)/<$0>/');
    like $expr, /'replacement_thunk: Some'/, 'an interpolating replacement carries the thunk';
}
else {
    skip 'the lowered shape is mutsu-specific', 3;
}
