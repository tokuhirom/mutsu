use Test;

# RakuAST::Term::True / RakuAST::Term::False build the same node as
# RakuAST::Term::Enum.from-identifier (#18982).

plan 10;

ok RakuAST::Term::True ~~ Mu, 'Term::True is a type name';
my $t = RakuAST::Term::True.new;
my $f = RakuAST::Term::False.new;
isa-ok $t, RakuAST::Term::Enum, '.new answers a Term::Enum node';
is $t.DEPARSE, 'True', 'True deparses';
is $f.DEPARSE, 'False', 'False deparses';
is-deeply $t.EVAL, True, 'True evaluates to True';
is-deeply $f.EVAL, False, 'False evaluates to False';
is $t.raku, "RakuAST::Term::Enum.from-identifier('True')", '.raku is the enum term';
dies-ok { RakuAST::Term::True.new(1) }, 'no arguments';

my $block = RakuAST::Doc::Block.new(:type<foo>);
$block.set-config({ numbered => RakuAST::Term::True.new; });
is-deeply $block.config.keys.List, ('numbered',), 'usable as a doc config value';
is $block.DEPARSE, "=begin foo :numbered\n\n=end foo\n\n", 'deparses as a flag';
