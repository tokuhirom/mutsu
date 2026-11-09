use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

plan 5;

# A `RakuAST::Statement::For` carrying an `otherwise` block runs it when the
# loop produced nothing (see rakuast-for-otherwise.t for the accessors).

sub say-block(Str $text) {
    RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name::WithoutParentheses.new(
            name => RakuAST::Name.from-identifier('push-to-log'),
            args => RakuAST::ArgList.new(RakuAST::StrLiteral.new($text)))))));
}

my @log;
sub push-to-log($text) { @log.push: $text }

my $empty = RakuAST::Statement::For.new(
    source => RakuAST::Var::Lexical.new('@none'), body => say-block('body'));
my @none;
$empty.replace-otherwise(say-block('otherwise'));
$empty.EVAL;
is-deeply @log, ['otherwise'], 'the otherwise block runs for an empty source';

my @some = 1;
my $full = RakuAST::Statement::For.new(
    source => RakuAST::Var::Lexical.new('@some'), body => say-block('body'));
$full.replace-otherwise(say-block('otherwise'));
@log = ();
$full.EVAL;
is-deeply @log, ['body'], 'it does not when the loop ran';

my $plain = RakuAST::Statement::For.new(
    source => RakuAST::Var::Lexical.new('@none'), body => say-block('body'));
@log = ();
$plain.EVAL;
is-deeply @log, [], 'a loop without the block does nothing for an empty source';

$empty.replace-otherwise(RakuAST::Block);
@log = ();
$empty.EVAL;
is-deeply @log, [], 'a type object clears the block';

$empty.replace-otherwise(say-block('again'));
@log = ();
$empty.EVAL;
is-deeply @log, ['again'], 'and it can be set again';
