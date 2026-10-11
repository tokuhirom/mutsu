use Test;
use nqp;

plan 11;

# nqp::ctxouter (#11498): the context of the scope enclosing a frame.
# Rakudo lowers uncaptured lexicals away, so each one a pad is asked for is
# captured by a closure.
my $top = 1;
my &keep-top = { $top };

sub plain() {
    my $mine = 2;
    my &keep = { $mine };
    my $c := nqp::ctx();
    my $o := nqp::ctxouter($c);
    ok !nqp::isnull($o), 'a routine has an enclosing context';
    ok nqp::existskey(nqp::ctxlexpad($c), '$mine'), 'its own pad holds its lexical';
    is nqp::atkey(nqp::ctxlexpad($o), '$top'), 1, 'the outer pad holds the program scope';
    nok nqp::existskey(nqp::ctxlexpad($o), '$mine'), 'the outer pad lacks the routine\'s lexical';
    my $oo := nqp::ctxouter($o);
    ok !nqp::isnull($oo), 'the program scope has an enclosing context too';
    nok nqp::existskey(nqp::ctxlexpad($oo), '$top'), 'and it holds no program lexical';
}
plain();

sub maker() {
    my $made = 5;
    my &keep = { $made };
    return sub {
        my $c := nqp::ctx();
        my $o := nqp::ctxouter($c);
        is nqp::atkey(nqp::ctxlexpad($o), '$made'), 5, 'a closure\'s outer is its creating scope';
        is nqp::atkey(nqp::ctxlexpad(nqp::ctxouter($o)), '$top'), 1, 'whose outer is the program scope';
    }
}
maker()();

my $mainline := nqp::ctx();
my $steps = 0;
my $walk := $mainline;
while !nqp::isnull($walk) && $steps < 10 {
    $walk := nqp::ctxouter($walk);
    $steps++;
}
ok $steps < 10, 'walking ctxouter from the mainline reaches null';
dies-ok { nqp::ctxouter(5) }, 'ctxouter of a non-context dies';

# A computed getlexouter name skips the running scope's own binding.
sub shadowing() {
    my $top = 2;
    my &keep = { $top };
    my $name = '$top';
    nqp::getlexouter($name)
}
is shadowing(), 1, 'a computed getlexouter name sees the enclosing scope, not the running one';
