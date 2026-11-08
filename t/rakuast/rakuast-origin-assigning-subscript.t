use Test;
use experimental :rakuast;

plan 13;

# The subscript of `%h<k> = v` / `@a[i] = v` runs through the assigned value
# (rakudo's assigning-subscript form); its brackets and key keep their own
# ranges (#15780).
sub range-of($node) { $node.origin ?? "{$node.origin.from}-{$node.origin.to}" !! 'none' }

my $hash = 'my %e; %e<M> = 1'.AST.statements[1].expression;
is range-of($hash), '7-16', 'the assignment spans the target and value';
my $sub = $hash.postfix;
isa-ok $sub, RakuAST::Postcircumfix::LiteralHashIndex, 'the angle subscript';
is range-of($sub), '9-16', 'the subscript runs through the value';
is range-of($sub.index), '10-11', 'the key is the text between the brackets';
is range-of($sub.index.segments[0]), '10-11', 'and its string segment';

my $words = 'my %e; %e<a b> = 1'.AST.statements[1].expression.postfix;
is range-of($words), '9-18', 'a word list';
is range-of($words.index), '10-13', 'whose quote spans the words';

my $array = 'my @a; @a[0] = 1'.AST.statements[1].expression.postfix;
isa-ok $array, RakuAST::Postcircumfix::ArrayIndex, 'the array subscript';
is range-of($array), '9-16', 'runs through the value';
is range-of($array.index), '10-11', 'its semilist is the index';
is range-of($array.index.statements[0]), '10-11', 'and its statement';

my $plain = 'my %e; %e<M>'.AST.statements[1].expression.postfix;
is range-of($plain), '9-12', 'a subscript without assignment is unchanged';
is range-of($plain.index), '10-11', 'and so is its key';
