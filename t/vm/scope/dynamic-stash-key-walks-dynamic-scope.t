use Test;

# `DYNAMIC::<key>` looks the key up exactly as spelled through the dynamic
# scope, frame by frame from the running one: a caller's ordinary lexical is
# found, a frame's own `my $*d` shadows the caller's from the frame's entry
# (unset before its declaration runs), and a process-level dynamic is in no
# frame (#13590). Checked against rakudo.
plan 7;

my $*d = 2;
my $x = 9;
my @list = 1, 2;
sub f {
    my @r;
    @r.push: DYNAMIC::<$*d>.raku;
    @r.push: DYNAMIC::<$*nope>.raku;
    @r.push: DYNAMIC::<$x>.raku;
    @r.push: DYNAMIC::<@list>.raku;
    @r.push: DYNAMIC::<$*OUT>.raku;
    my $*d = 5;
    @r.push: DYNAMIC::<$*d>.raku;
    @r
}
sub g { f() }
my @got = g();
is @got[0], 'Any', 'a frame\'s own dynamic is unset before its declaration runs';
is @got[1], 'Nil', 'an unbound dynamic';
is @got[2], '9', 'a caller\'s ordinary lexical';
is @got[3], '[1, 2]', 'a caller\'s array';
is @got[4], 'Nil', 'a process-level dynamic is in no frame';
is @got[5], '5', 'the frame\'s own dynamic after its declaration';
sub h { DYNAMIC::<$*d> }
is h(), 2, 'a callee without its own declaration sees the caller\'s';
