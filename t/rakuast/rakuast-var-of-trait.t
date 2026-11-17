use experimental :rakuast;
use Test;

# A variable's type written as an `of` trait after its name (`my @arr of Int`)
# is a `RakuAST::Trait::Of` in the declaration's `traits`, not its `type`
# (#14880, the `.DEPARSE` corpus's array-shapes.t).

plan 11;

sub deparse($src) { $src.AST.statements[*-1].DEPARSE }

is deparse(q{my @arr of Int = 1, 2, 3}), 'my @arr of Int = 1, 2, 3', 'an array';
is deparse(q{my @arr[5] of Int = <1 2 3 4 5>}), 'my @arr[5] of Int = <1 2 3 4 5>',
    'a shaped array';
is deparse(q{my %h of Str}), 'my %h of Str', 'a hash';
is deparse(q{my $x of Int is default(3)}), 'my $x of Int is default(3)',
    'before an is trait';
is deparse(q{my %h{Str} of Int}), 'my %h{Str} of Int', 'a key-typed hash';
is deparse(q{my Int @arr}), 'my Int @arr', 'a type before the name stays the type';

my $decl = Q{my @arr of Int}.AST.statements[0].expression;
isa-ok $decl.traits[0], RakuAST::Trait::Of, 'the trait is a Trait::Of';
nok $decl.raku.contains('type '), 'and the declaration has no written type';

my @arr of Int = 1, 2, 3;
is @arr.of, Int, 'the array is typed';
throws-like { @arr.push('x') }, X::TypeCheck, 'and checks its elements';
my %h{Str} of Int = a => 1;
is %h.of, Int, 'a key-typed hash too';
