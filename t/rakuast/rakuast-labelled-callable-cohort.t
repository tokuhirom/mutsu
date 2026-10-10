use Test;
use experimental :rakuast;

sub run-tree(Str $source) { EVAL($source.AST) }

my $labelled = Q{OUTER: do { 42 }}.AST.statements[0];
is $labelled.labels[0].name, 'OUTER', 'label exposes its named field';
is $labelled.expression.^name, 'RakuAST::StatementPrefix::Do',
    'labelled do retains its prefix';
is Q{OUTER: { 42 }}.AST.statements[0].expression.^name, 'RakuAST::Block',
    'labelled bare block retains its spelling';
is run-tree(Q{my $f = sub { do L: { L.leave(42) } + 100 }; $f()}), 142,
    'a labelled block is still a leave target in expression context';
is run-tree(Q{my $x = 2; L: do { my $x = 9 }; $x}), 2,
    'labelled do keeps lexical scope';
is run-tree(Q{my $x = 2; L: { my $x = 9 }; $x}), 2,
    'labelled bare block keeps lexical scope';
dies-ok { run-tree(Q{L: do { last L }}) },
    'a labelled do does not become a loop';
dies-ok { run-tree(Q{L: { next L }}) },
    'a labelled bare block does not become a loop';
is run-tree(Q{my $n = 0; OUTER: for 1..3 { for 1..3 { $n++; next OUTER } }; $n}), 3,
    'resolved label terms retain labelled loop control';
is run-tree(Q{my $n = 0; OUTER: for 1..3 { $n++; (last OUTER) }; $n}), 1,
    'resolved label terms retain expression-position loop control';

# Inspect the standalone compiler variables through a routine body too.
my $routine = Q{sub recurse { &?ROUTINE }}.AST.statements[0].expression;
is $routine.body.statement-list.statements[0].expression.^name,
    'RakuAST::Var::Compiler::Routine', 'routine reference uses a compiler variable';
is run-tree(Q{sub recurse($n) { $n > 1 ?? &?ROUTINE($n - 1) !! $n }; recurse(4)}), 1,
    'routine compiler reference retains recursive callable identity';
my $block-call = Q{sub callback { 1.&?BLOCK }}.AST.statements[0].expression.body.statement-list.statements[0].expression.postfix;
is $block-call.^name, 'RakuAST::Call::BlockMethod', 'special callable method keeps its node class';
is $block-call.block.^name, 'RakuAST::Var::Compiler::Block', 'block method exposes the current block';
ok $block-call.block ~~ RakuAST::Term, 'compiler block reference is a term';
ok RakuAST::Var::Compiler::Block ~~ RakuAST::Term, 'compiler variable type has the same ancestry';
is run-tree(Q{(4,).map({ $_ > 1 ?? ($_ - 1).&?BLOCK !! $_ }).join}), '1',
    'block method preserves recursive callback identity';
is-deeply run-tree(Q{my @seen; for 2 { @seen.push($_); ($_ - 1,)».&?BLOCK if $_ > 0 }; @seen}),
    [2, 1, 0], 'hyper block method preserves the current for block';
is run-tree(Q{my &*f = -> $v { $v + 2 }; 3.&*f}), 5,
    'block method resolves a dynamic callable';

my $bound-code = Q{my &callback := { $^x + 2 }}.AST.statements[0].expression;
is $bound-code.initializer.^name, 'RakuAST::Initializer::Bind',
    'callable declaration retains binding intent';
is $bound-code.initializer.expression.^name, 'RakuAST::Block',
    'bound callable retains its block spelling';
is run-tree(Q{my &callback := { $^x + 2 }; callback(5)}), 7,
    'bound block keeps its placeholder signature';
is run-tree(Q{my &callback := -> $x { $x * 2 }; callback(5)}), 10,
    'bound pointy block keeps its signature';
is run-tree(Q{sub source($x) { $x + 3 }; my &callback := &source; callback(5)}), 8,
    'callable-to-callable binding keeps the original code value';
is run-tree(Q{my &callback = -> $x { $x + 4 }; callback(5)}), 9,
    'callable assignment remains distinct from binding';

my $label = RakuAST::Label.new(name => 'BUILT');
my $built = RakuAST::Statement::Expression.new(
    labels => ($label,),
    expression => RakuAST::Block.new,
);
lives-ok { EVAL($built) }, 'constructed labelled statement lowers';
is $built.labels[0].name, 'BUILT', 'constructed label exposes its name';
nok RakuAST::Statement::Expression.^methods(:local).map(*.name).grep(* eq 'labels'),
    'labels is not a local statement-expression method';
ok RakuAST::Statement::Expression.^methods.map(*.name).grep(* eq 'labels'),
    'labels is an inherited statement method';
is RakuAST::Statement.^attributes(:local).map(*.name).join(','), '$!labels',
    'the Statement model owns the labels attribute';
is RakuAST::Statement::Expression.^attributes.grep(*.name eq '$!labels')[0].package.^name,
    'RakuAST::Statement', 'inherited label attribute keeps its declaring package';
is RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)).labels.elems,
    0, 'an unlabelled statement inherits an empty labels list';
my $leave-built = RakuAST::ApplyPostfix.new(
    operand => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('BUILT')),
    postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('leave'),
        args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(42)),
    ),
);
my $built-body = RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $leave-built),
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(99)),
);
my $built-leave = RakuAST::Statement::Expression.new(
    labels => ($label,),
    expression => RakuAST::Block.new(body => RakuAST::Blockoid.new($built-body)),
);
is EVAL($built-leave), 42, 'constructed label terms resolve to their enclosing leave target';
my $outside-label = try EVAL(RakuAST::Statement::Expression.new(
    expression => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('BUILT')),
));
nok $outside-label ~~ Label, 'constructed label scope does not leak into a later lowering';
my $conditional = RakuAST::Statement::Expression.new(
    expression => RakuAST::IntLiteral.new(42),
    condition-modifier => Q{42 if False}.AST.statements[0].condition-modifier,
);
nok EVAL($conditional).defined, 'constructed statement retains its conditional modifier';

is run-tree(Q{use lib 't/lib'; use ExportHookTermVsTaggedSub; t.hi}), 'hi',
    'an export hook term wins over a tagged routine';
is run-tree(Q{use lib 't/lib'; use ExportHookTermVsTaggedSub; t}).hi, 'hi',
    'a final bare statement keeps the deferred term choice';
is EVAL(Q{use lib 't/lib'; use ExportHookTermVsTaggedSub; t}).hi, 'hi',
    'ordinary execution also keeps a final bare hook term';
is run-tree(Q{use lib 't/lib'; use ExportHookTermVsTaggedSub :t; t()}), 'from-sub',
    'parentheses still select the tagged routine';
is run-tree(Q{use lib 't/lib'; use ExportHookOtherTerm :u; u}), 'from-sub-u',
    'deferred bare call falls back to the imported routine';
is run-tree(Q{use lib 't/lib'; use ExportHookOtherTerm :u; other}), 42,
    'deferred import keeps the hook-installed term';
is run-tree(Q{use lib 't/lib'; use ExportHookShadowsTermKeyword; True.Str}), 'Tri(1)',
    'a folded keyword retains the export hook choice through lowering';
nok run-tree(Q{use lib 't/lib'; use ExportHookShadowsTermKeyword; Nil.defined}),
    'an unshadowed keyword retains its fallback value';
is run-tree(Q{use lib 't/lib'; use ExportHookOtherTerm :u; Int}).^name, 'Int',
    'a known type retains its resolution after a dynamic export hook';
is Q{use lib 't/lib'; use ExportHookOtherTerm :u; Int}.AST.statements[*-1].expression.^name,
    'RakuAST::Type::Simple', 'dynamic imports do not change a known type node class';
dies-ok { run-tree(Q{use lib 't/lib'; use ExportHookOtherTerm :u; absent-hook-term()}) },
    'an unresolved explicit call still fails at execution';

my &*constructed-call = -> $x { $x * 3 };
my $call = RakuAST::ApplyPostfix.new(
    operand => RakuAST::IntLiteral.new(4),
    postfix => RakuAST::Call::BlockMethod.new(
        block => RakuAST::Var::Dynamic.new('&*constructed-call'),
    ),
);
is EVAL($call), 12, 'constructed block method uses ordinary dynamic dispatch';

done-testing;
