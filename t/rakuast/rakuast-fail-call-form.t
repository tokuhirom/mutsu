use experimental :rakuast;
use Test;

# `fail(...)` written as a call, measured on rakudo 2026.11: `.AST` keeps a
# `Call::Name` over the arguments (not `fail ("...")`), with its origins, and
# the routine still fails with them.

plan 6;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[sub f { fail("nope") }]),
    'Sub@0-22 Name@4-5 Blockoid@6-22 Call::Name@8-20 Name@8-12 ArgList@13-19 QuotedString@13-19 StrLiteral@14-18',
    'fail("nope") is a placed Call::Name';
is Q[fail("a", "b")].AST.statements[0].expression.args.args.elems, 2,
    'its arguments are the call arguments';
is Q[fail("a", "b")].AST.DEPARSE, 'fail("a", "b")', 'it deparses as a call';

sub f { fail("nope") }
sub g { fail("a", "b") }
sub k { fail("x") if True; 5 }
is f().exception.message, 'nope', 'fail("nope") fails with its argument';
is g().exception.message, 'ab', 'several arguments are joined';
is k().exception.message, 'x', 'a statement modifier still applies';
