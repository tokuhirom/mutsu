use Test;

# `EVAL` of a hand-built tree holding `RakuAST::Doc::Block` statements with
# configuration, a margin, table rows or the `=config` directive establishes
# `$=pod` too (#14880, the write-back of rakuast-doc-block-pod.t): the block is
# written back as Pod source and read like a source string. Shapes compared
# with rakudo 2026.09.

plan 10;

use experimental :rakuast;

sub pod-of($block) {
    EVAL RakuAST::StatementList.new($block,
        RakuAST::Statement::Expression.new(expression => RakuAST::Var::Doc.new("pod")))
}
sub flag($name) { $name => RakuAST::Term::Enum.from-identifier('True') }
sub off($name) { $name => RakuAST::Term::Enum.from-identifier('False') }
sub nconf($name, $n) {
    $name => RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new($n))))
}
sub conf($pod) { $pod[0].config.sort.map({ .key ~ "=" ~ .value.raku }).join(",") }

my $margin = pod-of(RakuAST::Doc::Block.new(margin => "  ", type => "pod", paragraphs => ("hello\n",)));
is $margin.elems, 1, 'a margin: one block';
is $margin[0].contents[0].contents.join, "hello", 'the margin is taken off the text';

my $flag = pod-of(RakuAST::Doc::Block.new(type => "foo", config => (flag("a"),), paragraphs => ("x\n",)));
is conf($flag), "a=Bool::True", 'a flag';
is $flag[0].name, "foo", 'its name';

my $off = pod-of(RakuAST::Doc::Block.new(type => "foo", config => (off("b"),), paragraphs => ("x\n",)));
is conf($off), "b=Bool::False", 'a negated flag';

my $num = pod-of(RakuAST::Doc::Block.new(type => "foo", config => (nconf("n", 3),), paragraphs => ("x\n",)));
is conf($num), "n=3", 'an integer';

my $table = pod-of(RakuAST::Doc::Block.new(type => "pod", paragraphs => (
    RakuAST::Doc::Block.new(type => "table", paragraphs => (
        RakuAST::Doc::LegacyRow.new(column-offsets => (0, 3), cells => ("A", "B")),
        RakuAST::Doc::LegacyRow.new(column-offsets => (0, 3), cells => ("C", "D")))),)));
is $table[0].contents[0].^name, "Pod::Block::Table", 'a table block';
is $table[0].contents[0].contents.raku, '[["A", "B"], ["C", "D"]]', 'its rows';

my $config = pod-of(RakuAST::Doc::Block.new(type => "config", directive => True, paragraphs => ("head1",), config => (flag("numbered"),)));
is $config[0].^name, "Pod::Config", 'the =config directive';

# The EVAL's $=pod is its own: the caller's is not touched.
my $before = $=pod.elems;
pod-of(RakuAST::Doc::Block.new(type => "foo", paragraphs => ("x\n",)));
is $=pod.elems, $before, 'the caller\'s $=pod is left alone';
