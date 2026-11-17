use experimental :rakuast;
use nqp;
use Test;

# `nqp::getattr` / `nqp::bindattr` on a RakuAST node's attribute read and write
# the node's field in place, seen by every holder of the node (#17327). Red's
# CHECK-time `red-compile` rewrites the unit's tree this way:
# `nqp::bindattr(nqp::decont($apply), RakuAST::ApplyDottyInfix, '$!left', ...)`.

plan 10;

CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::ApplyDottyInfix && $n.left ~~ RakuAST::IntLiteral
            && $n.left.value == 41 {
            nqp::bindattr(nqp::decont($n), RakuAST::ApplyDottyInfix, '$!left',
                RakuAST::IntLiteral.new(42));
        }
        if $n ~~ RakuAST::IntLiteral && $n.value == 1007 {
            nqp::bindattr(nqp::decont($n), RakuAST::Literal, '$!value', 1008);
        }
        $n.visit-children(&walk);
    }
    walk($*CU);
}

{
    my $i = RakuAST::IntLiteral.new(5);
    my $alias = $i;
    is nqp::getattr(nqp::decont($i), RakuAST::Literal, '$!value'), 5,
        'getattr reads a literal\'s $!value';
    nqp::bindattr(nqp::decont($i), RakuAST::Literal, '$!value', 7);
    is $i.value, 7, 'bindattr writes the literal\'s value';
    is $alias.value, 7, 'another holder of the node sees the bind';
}

{
    my $dotty = RakuAST::ApplyDottyInfix.new(
        left  => RakuAST::IntLiteral.new(1),
        infix => RakuAST::DottyInfix::Call.new,
        right => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('Str')));
    my $statement = RakuAST::Statement::Expression.new(expression => $dotty);
    is nqp::getattr(nqp::decont($dotty), RakuAST::ApplyDottyInfix, '$!left').value, 1,
        'getattr reads a child node attribute';
    nqp::bindattr(nqp::decont($dotty), RakuAST::ApplyDottyInfix, '$!left',
        RakuAST::IntLiteral.new(9));
    is $statement.expression.left.value, 9, 'the bind is seen through the parent node';
    is EVAL($dotty), '9', 'EVAL of the rewritten node runs the new child';
}

{
    # The issue's reduction: an `ArgList.push`, then a bind on the parent.
    my $call = RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('grep'),
        args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(1)));
    $call.args.push: RakuAST::ColonPair::Value.new(:key<red-ast>,
        :value(RakuAST::IntLiteral.new(2)));
    is $call.args.args.elems, 2, 'ArgList.push appends';
    my $dotty = RakuAST::ApplyDottyInfix.new(
        left => RakuAST::IntLiteral.new(1), infix => RakuAST::DottyInfix::Call.new,
        right => $call);
    nqp::bindattr(nqp::decont($dotty), nqp::decont(RakuAST::ApplyDottyInfix), '$!left',
        RakuAST::IntLiteral.new(9));
    is $dotty.left.value, 9, 'bindattr with a decont-ed class operand';
}

# The unit runs the tree its CHECK phaser rewrote.
is 41 .Str, '42', 'a CHECK-time bind of $!left changes what runs';
is 1007, 1008, 'a CHECK-time bind of a literal\'s $!value changes what runs';
