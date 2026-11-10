use Test;
use experimental :rakuast;
use nqp;
use lib 't/fixtures/check-rewrite-export-type/lib';
use RewriteTarget;

# A CHECK-phase closure on `$*CU` that rewrites the tree makes the unit continue
# from the rewritten tree (rakudo compiles the tree, not the text). Identical
# under rakudo.
plan 4;

my @log;
sub marker(*@a) { @log.push(@a.join(",")) }

BEGIN {
    my $phasers := nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers');
    nqp::push($phasers, {
        my (@marks, @gadgets);
        sub walk($parent, $node) {
            @marks.push($node)
                if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'marker';
            if $node ~~ RakuAST::ApplyPostfix && $node.postfix ~~ RakuAST::Call::Term {
                @gadgets.push(($parent, $node))
                    if $node.operand ~~ RakuAST::Type::Simple && $node.operand.DEPARSE eq 'Gadget';
            }
            my @kids;
            $node.visit-children(-> $c { @kids.push($c) }) if $node.can('visit-children');
            walk($node, $_) for @kids;
        }
        walk($*CU, $*CU);
        .args.push(RakuAST::StrLiteral.new("rewritten")) for @marks;
        for @gadgets -> ($parent, $node) {
            my $call = RakuAST::ApplyPostfix.new(
                operand => RakuAST::Term::Declaration.new(
                    RakuAST::Declaration::ResolvedConstant.new(
                        :compile-time-value(Gadget.new(n => 42)))),
                postfix => RakuAST::Call::Term.new);
            $parent.IMPL-REPLACE-CHILD($node, $call);
        }
    });
}

marker("orig");
is-deeply @log, ["orig,rewritten"], 'a call added an argument at CHECK time';
is Gadget.(), "gadget 42", 'an imported type is a Type::Simple, and a node swapped at CHECK time with IMPL-REPLACE-CHILD';
is @log.elems, 1, 'the unit body ran once';
my $x = Gadget.();
is $x, "gadget 42", 'a second call site is rewritten too';
