use Test;

# `&f` builds a code object for a registered routine. Its captured env is
# filtered like a closure capture (free variables plus system names) rather
# than sharing the whole live env (#9169). These pin what the routine must
# still see when it is called through that code object.

plan 14;

my $x = 1;
sub f { $x }
my &g = &f;
$x = 2;
is g(), 2, '&f reads a mainline lexical at call time, not at capture time';

sub mk { my $y = 5; sub inner { $y }; &inner }
is mk()(), 5, 'a named sub escaping its declaring routine keeps its free var';

sub mk3($n) { sub inner3 { $n * 2 }; &inner3 }
is mk3(21)(), 42, 'a named sub closes over the enclosing routine parameter';

sub fd { $*d }
my $*d = 3;
my $c = &fd;
sub h($c) { my $*d = 4; $c() }
is h($c), 4, 'a dynamic variable resolves against the caller chain';

sub mk2 { my sub a { 7 }; sub b { a() }; &b }
is mk2()(), 7, 'a lexical sub called by name from the escaping routine';

is (await start { &f() }), 2, 'a code object called from another thread';

is &f.name, 'f', '.name survives';
sub typed(--> Int) { 42 }
is &typed.returns.^name, 'Int', 'the return type rides on the code object';

my @l = 1, 2, 3;
sub usel { @l.elems }
my $ul = &usel;
@l.push(4);
is $ul(), 4, 'a captured array is the live container';

sub counter { state $n = 0; ++$n }
my $cn = &counter;
$cn();
$cn();
is counter(), 3, 'state is shared between &f calls and by-name calls';

sub rec($n) { $n <= 0 ?? 0 !! $n + &rec($n - 1) }
is rec(4), 10, '&rec inside its own body recurses';

{
    my $z = 'blk';
    sub inb { $z }
    my $ib = &inb;
    is $ib(), 'blk', 'a sub declared in a bare block sees the block lexical';
}

sub outerf { my $w = 'w1'; my sub innerf { $w }; my $r = &innerf; $w = 'w2'; $r() }
is outerf(), 'w2', 'a later write to the captured lexical is visible';

{
    # Big frame: many unrelated lexicals must not change what `&f` sees.
    my $a1 = 1; my $a2 = 2; my $a3 = 3; my $a4 = 4; my $a5 = 5;
    my $s = 0;
    for ^100 { my &q = &f; $s += q() }
    is $s, 200, '&f read in a loop inside a frame with many lexicals';
}
