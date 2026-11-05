use Test;

# #16762: a RakuAST constructor reads a repeated named argument like a Raku
# call does -- the last one wins -- so the idiom `.new(|%defaults, |%overrides)`
# works. mutsu kept the first.

plan 9;

sub s($v) { RakuAST::StrLiteral.new($v) }

is RakuAST::Doc::Block.new(type => 'para', type => 'code', paragraphs => ()).type, 'code',
    'Doc::Block: the last type wins';
is RakuAST::Doc::Markup.new(letter => 'B', letter => 'I', atoms => ('x',)).letter, 'I',
    'Doc::Markup: the last letter wins';

# the defaults-then-overrides idiom
sub blk(*%h) { RakuAST::Doc::Block.new(type => 'para', paragraphs => (), |%h) }
is blk(type => 'code').type, 'code', 'overrides given through a slurpy hash win';
is blk().type, 'para', 'and the default stays when there is no override';

my %over = type => 'defn';
is RakuAST::Doc::Block.new(type => 'para', |%over).type, 'defn', 'a flattened hash after a named argument wins';

is RakuAST::ColonPair::Value.new(key => 'k', key => 'z', value => s('v')).key, 'z',
    'ColonPair::Value: the last key wins';
my $infix = RakuAST::ApplyInfix.new(
    left => s('a'), infix => RakuAST::Infix.new('+'), right => s('b'), right => s('c'));
is $infix.right.value, 'c', 'ApplyInfix: the last right operand wins';

# a name given once is unaffected, whatever its position
my $once = RakuAST::ApplyInfix.new(right => s('b'), infix => RakuAST::Infix.new('+'), left => s('a'));
is $once.left.value ~ $once.right.value, 'ab', 'a name given once is read as before';

is RakuAST::Doc::Block.new(type => 'para', type => 'code', paragraphs => ()).raku.contains('"code"'), True,
    'and the node prints the winning value';
