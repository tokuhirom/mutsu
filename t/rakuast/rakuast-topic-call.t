use Test;
use experimental :rakuast;

# Write direction of `RakuAST::Call::Method`, `RakuAST::Term::TopicCall` and
# `RakuAST::Term::Whatever` construction (Needle::Compile's `make-method`
# builds `.method(...)` on the topic this way). Read direction is not covered:
# the parser does not yet tell `.uc` from `$_.uc`.

plan 13;

my $m = RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier("uc"));
is $m.^name, 'RakuAST::Call::Method', 'Call::Method.new builds the node';
is $m.name.^name, 'RakuAST::Name', '...with its name';
is $m.args.^name, 'RakuAST::ArgList', '...and an empty ArgList by default';

my $t = RakuAST::Term::TopicCall.new($m);
is $t.^name, 'RakuAST::Term::TopicCall', 'Term::TopicCall.new takes the call positionally';
is $t.call.^name, 'RakuAST::Call::Method', '...and answers it from .call';
ok $t ~~ RakuAST::Term, 'a TopicCall is a Term';
is $t.raku, q:to/RAKU/.chomp, '.raku renders the constructor';
RakuAST::Term::TopicCall.new(
  RakuAST::Call::Method.new(
    name => RakuAST::Name.from-identifier("uc")
  )
)
RAKU

my $block = RakuAST::PointyBlock.new(
  signature => RakuAST::Signature.new(
    parameters => (
      RakuAST::Parameter.new(
        target => RakuAST::ParameterTarget::Var.new(name => '$_')
      ),
    )
  ),
  body => RakuAST::Blockoid.new(
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(expression => $t)
    )
  )
);
is $block.EVAL()("abc"), "ABC", 'EVAL calls the method on the topic';

my $with-args = RakuAST::Call::Method.new(
  name => RakuAST::Name.from-identifier("contains"),
  args => RakuAST::ArgList.new(RakuAST::StrLiteral.new("b"))
);
is $with-args.args.args.elems, 1, 'Call::Method keeps its args';
given "abc" {
    ok RakuAST::Term::TopicCall.new($with-args).EVAL, 'a TopicCall with args runs';
}
given "xyz" {
    nok RakuAST::Term::TopicCall.new($with-args).EVAL, '...against the current topic';
}

my $w = RakuAST::Term::Whatever.new;
is $w.^name, 'RakuAST::Term::Whatever', 'Term::Whatever.new builds the node';
is $w.raku, 'RakuAST::Term::Whatever.new', '...rendered without parens';
