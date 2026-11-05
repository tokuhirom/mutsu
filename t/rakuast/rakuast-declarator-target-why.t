use Test;
use experimental :rakuast;

# #16773: a declaration built with `WHY => $doc` keeps the documentation, `WHY`
# answers it (the `Doc::Declarator` type object when there is none), `set-WHY`
# replaces it, and `.raku` leaves it out. mutsu accepted the argument and lost
# it.

plan 27;

my $name = RakuAST::Name.from-identifier('S');
my $x = RakuAST::Var::Lexical.new('$x');
my $doc = RakuAST::Doc::Declarator.new(WHEREFORE => $x, leading => ('lead',));
my $empty-body = RakuAST::Blockoid.new(RakuAST::StatementList.new);

my %builders =
    Subset    => { RakuAST::Type::Subset.new(name => $name, |%_) },
    Sub       => { RakuAST::Sub.new(name => $name, |%_) },
    Method    => { RakuAST::Method.new(name => $name, |%_) },
    Submethod => { RakuAST::Submethod.new(name => $name, |%_) },
    Class     => { RakuAST::Class.new(name => $name, |%_) },
    Role      => { RakuAST::Role.new(name => $name, |%_) },
    Package   => { RakuAST::Package.new(name => $name, |%_) },
    Grammar   => { RakuAST::Grammar.new(name => $name, |%_) },
    Block     => { RakuAST::Block.new(body => $empty-body, |%_) };

for %builders.sort(*.key) -> (:key($class), :value(&build)) {
    my $with = build(WHY => $doc);
    my $without = build();
    ok $with.WHY.defined && $with.WHY ~~ RakuAST::Doc::Declarator, "$class keeps WHY";
    nok $without.WHY.defined, "$class without WHY answers the type object";
}

my $subset = RakuAST::Type::Subset.new(name => $name, WHY => $doc);
is $subset.WHY.leading.raku, '("lead",)', 'the stored node is the one given';
nok $subset.raku.contains('WHY'), '.raku does not print WHY itself';
is $subset.raku, RakuAST::Type::Subset.new(name => $name).raku ~ qq[.declarator-docs(\n  leading => (\n    "lead",\n  )\n)],
    '.raku prints the documentation as a declarator-docs call';
my $both = RakuAST::Doc::Declarator.new(WHEREFORE => $x, leading => ('a', 'b'), trailing => ('c',));
is RakuAST::Sub.new(name => $name, WHY => $both).raku.lines.skip(5).join('|'),
    ').declarator-docs(|  leading  => (|    "a",|    "b",|  ),|  trailing => (|    "c",|  )|)',
    'leading and trailing lines print aligned';

my $sub = RakuAST::Sub.new(name => $name);
nok $sub.WHY.defined, 'no WHY before set-WHY';
is $sub.set-WHY($doc).raku, 'Nil', 'set-WHY answers Nil';
is $sub.WHY.leading.raku, '("lead",)', 'set-WHY stores the documentation';
$sub.set-WHY(RakuAST::Doc::Declarator);
ok $sub.WHY.defined, 'a type object leaves the stored documentation alone';

my $msg = '';
try { RakuAST::Sub.new(name => $name, WHY => 5); CATCH { default { $msg = .message } } }
like $msg, /"Type check failed in binding to parameter '\$WHY'; expected RakuAST::Doc::Declarator but got Int"/,
    'a WHY that is not a Doc::Declarator is refused';
