use v6;
use Test;

# A statement prefix over a bare statement keeps the statement in `.AST`
# (ADR-12199, #12199): rakudo writes `gather say 1` as
# `StatementPrefix::Gather(Statement::Expression(...))`, where the parser's own
# tree is the block `gather { say 1 }` makes.
# This file passes under BOTH mutsu and raku, so raku is the oracle.

plan 59;

sub init-of(Str $source) {
    $source.AST.statements[*-1].expression.initializer.expression;
}

# --- read direction: the bare form holds a statement, the braced form a block ---
for <gather start try once do> -> $kw {
    my $class = "RakuAST::StatementPrefix::" ~ $kw.tclc;
    my $bare = init-of("my \$x = $kw say 1;");
    is $bare.^name, $class, "$kw STATEMENT is a $class";
    isa-ok $bare.blorst, RakuAST::Statement::Expression, "$kw STATEMENT holds a statement";
    next if $kw eq 'do';
    my $braced = init-of("my \$x = $kw \{ say 1 \};");
    isa-ok $braced.blorst, RakuAST::Block, "$kw \{ BLOCK } still holds a block";
}

{
    my $p = init-of('my $x = BEGIN say 1;');
    isa-ok $p, RakuAST::StatementPrefix::Phaser::Begin, 'BEGIN STATEMENT is a Phaser::Begin';
    isa-ok $p.blorst, RakuAST::Statement::Expression, 'BEGIN STATEMENT holds a statement';
    isa-ok init-of('my $x = BEGIN { 1 };').blorst, RakuAST::Block, 'BEGIN { BLOCK } holds a block';
}

# --- the statement is whatever was written ---
{
    my $g = init-of('my @a; my $s = gather @a.map: *.take;');
    isa-ok $g.blorst, RakuAST::Statement::Expression, 'gather @a.map: *.take';
    isa-ok $g.blorst.expression, RakuAST::ApplyPostfix, 'the statement is the method call';
}
isa-ok init-of('my $x = gather for 1, 2 { .take };').blorst, RakuAST::Statement::For,
    'gather for is a for statement';
isa-ok init-of('my $x = do for 1, 2 { .say };').blorst, RakuAST::Statement::For,
    'do for is a for statement';
isa-ok init-of('my $x = do if 1 { 2 };').blorst, RakuAST::Statement::If,
    'do if is an if statement';
isa-ok init-of('my $x = try say 1;').blorst.expression, RakuAST::Call::Name::WithoutParentheses,
    'try STATEMENT holds the bare call';

# --- statement level: a phaser over a bare statement ---
sub stmt-of(Str $source) { $source.AST.statements[0].expression }
for <LEAVE ENTER END FIRST NEXT LAST> -> $kw {
    my $class = "RakuAST::StatementPrefix::Phaser::" ~ $kw.tclc;
    my $bare = stmt-of("$kw say 1");
    is $bare.^name, $class, "$kw STATEMENT is a $class";
    unlike $bare.gist, /'RakuAST::Block'/, "$kw STATEMENT holds no block";
    like $bare.gist, /'RakuAST::Statement::Expression.new(' \s+ 'expression => RakuAST::Call'/,
        "$kw STATEMENT holds the statement";
    like stmt-of("$kw \{ say 1 \}").gist, /'RakuAST::Block'/, "$kw \{ BLOCK } still holds a block";
}
# --- write direction ---
{
    my $ast = RakuAST::StatementPrefix::Gather.new(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name::WithoutParentheses.new(
                name => RakuAST::Name.from-identifier('take'),
                args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(7)))));
    is-deeply EVAL($ast).list, (7,), 'EVAL of a hand-built gather over a statement';
}
{
    my $ast = RakuAST::StatementPrefix::Try.new(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name::WithoutParentheses.new(
                name => RakuAST::Name.from-identifier('die'),
                args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('x')))));
    is EVAL($ast), Nil, 'EVAL of a hand-built try over a statement';
}
{
    my $ast = RakuAST::StatementPrefix::Do.new(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(5)));
    is EVAL($ast), 5, 'EVAL of a hand-built do over a statement';
}

{
    my $ast = RakuAST::StatementPrefix::Phaser::Leave.new(
        RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name::WithoutParentheses.new(
                name => RakuAST::Name.from-identifier('say'),
                args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('left')))));
    my $code = RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $ast));
    is-deeply EVAL($code).defined, False, 'EVAL of a hand-built LEAVE over a statement';
}

# --- the bare forms still run ---
is-deeply EVAL('(gather take 3).list'), (3,), 'gather STATEMENT runs';
is EVAL('do 5 + 1'), 6, 'do STATEMENT runs';
is EVAL('try die "x"; 1'), 1, 'try STATEMENT runs';
is EVAL('my $p = start 40 + 2; await $p'), 42, 'start STATEMENT runs';
is EVAL('my $x = 0; for 1..3 { once $x++ }; $x'), 1, 'once STATEMENT runs';
is EVAL('my $r = 0; for 1..3 { NEXT $r += 10; }; $r'), 30, 'NEXT STATEMENT runs';
is EVAL('my $r = 0; for 1..3 { FIRST $r = 7; }; $r'), 7, 'FIRST STATEMENT runs';
is EVAL('my $r = 0; for 1..3 { LAST $r = $_; }; $r'), 3, 'LAST STATEMENT runs';
