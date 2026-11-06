use Test;
use nqp;

plan 8;

# Code compiled through the compiler object (what a REPL sandbox does) sees
# the CALLER's dynamic variables, and in particular a redirected `$*OUT`
# (#16947).
my $comp := nqp::getcomp('Raku');
sub with-dyn($code) {
    my $*FOO = 42;
    my $buf = '';
    my $*OUT = $*OUT but role { method print(*@a) { $buf ~= @a.join; True } };
    $comp.eval($code);
    $buf;
}
is with-dyn('say $*FOO; print "x"'), "42\nx", 'say/print go to the caller\'s $*OUT, $*FOO is visible';
is with-dyn('print "a"'), 'a', 'a second call sees its own caller\'s $*OUT';

# A unit whose LAST statement assigns to a sigilless term yields the assigned
# value, as the parenthesized form does.
my \p = do { state $l; Proxy.new( FETCH => method () { $l }, STORE => method ($x) { $l = $x } ) };
is (EVAL 'p = 8'), 8, 'EVAL of a bare sigilless assignment yields the value';
is (EVAL '(p = 9)'), 9, 'the parenthesized form agrees';
is (EVAL 'my $q = 3; p = $q'), 3, 'a unit ending in a sigilless assignment after a declaration';
is p, 3, 'the store reached the Proxy';

my $v = 1;
is (EVAL '$v = 5'), 5, 'a scalar assignment tail is unchanged';
my \plain = 1;
dies-ok { EVAL 'plain = 2' }, 'a plain sigilless term stays readonly';
