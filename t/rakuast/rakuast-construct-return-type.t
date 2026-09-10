use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST construction of signature return types and return traits.
# The resulting nodes must remain usable by the existing lowerer, not just
# render like Rakudo's model.

plan 19;

my $type = RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier('Int'),
);

my $signature = RakuAST::Signature.new(
    parameters => (),
    returns => $type,
);
isa-ok $signature, RakuAST::Signature,
    'Signature.new constructs a signature with a return type';
is $signature.returns, $type,
    'Signature.returns exposes the constructed type node';
is $signature.returns.name.gist, 'RakuAST::Name.from-identifier("Int")',
    'the return type remains walkable';
is $signature.gist, q:to/END/.chomp, 'Signature.gist renders returns like Rakudo';
    RakuAST::Signature.new(
      parameters => $( ),
      returns    => RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier("Int")
      )
    )
    END
my @signature-methods = RakuAST::Signature.^methods(:local)>>.name;
ok 'new' (elem) @signature-methods && 'returns' (elem) @signature-methods,
    'Signature introspection exposes its constructor and return accessor';

my $returns-trait = RakuAST::Trait::Returns.new($type);
isa-ok $returns-trait, RakuAST::Trait::Returns,
    'Trait::Returns.new constructs a return trait';
is $returns-trait.type, $type,
    'Trait::Returns.type exposes the constructed type node';
is $returns-trait.gist, q:to/END/.chomp, 'Trait::Returns.gist renders like Rakudo';
    RakuAST::Trait::Returns.new(
      RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier("Int")
      )
    )
    END

my $of-trait = RakuAST::Trait::Of.new($type);
isa-ok $of-trait, RakuAST::Trait::Of,
    'Trait::Of.new constructs an of trait';
is $of-trait.type, $type,
    'Trait::Of.type exposes the constructed type node';
is $of-trait.gist, q:to/END/.chomp, 'Trait::Of.gist renders like Rakudo';
    RakuAST::Trait::Of.new(
      RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier("Int")
      )
    )
    END
ok RakuAST::Trait::Returns.^can('new').elems > 0
    && RakuAST::Trait::Of.^can('new').elems > 0,
    'return traits advertise their constructors';

sub body($value) {
    my $statements = RakuAST::StatementList.new;
    $statements.add-statement(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new($value),
        ),
    );
    RakuAST::Blockoid.new($statements);
}

my $arrow-sub = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('rakuast-constructed-arrow-return'),
    signature => $signature,
    body => body(42),
);
is $arrow-sub.signature.returns, $type,
    'Sub.new retains a constructed Signature.returns node';
my $arrow-callable = EVAL($arrow-sub);
is $arrow-callable(), 42,
    'a constructed Signature.returns lowers and executes';

my $trait-sub = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('rakuast-constructed-returns-trait'),
    traits => [$returns-trait],
    body => body(42),
);
is $trait-sub.traits[0], $returns-trait,
    'Sub.new retains a constructed Trait::Returns node';
ok $trait-sub.gist.contains('traits'),
    'Sub.gist renders constructed return traits';
my $trait-callable = EVAL($trait-sub);
is $trait-callable(), 42,
    'a constructed Trait::Returns lowers and executes';

my $of-sub = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('rakuast-constructed-of-trait'),
    traits => [$of-trait],
    body => body(42),
);
is $of-sub.traits[0], $of-trait,
    'Sub.new retains a constructed Trait::Of node';
my $of-callable = EVAL($of-sub);
is $of-callable(), 42,
    'a constructed Trait::Of lowers and executes';
