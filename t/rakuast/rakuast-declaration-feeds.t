use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

for Q[my @a <== (1, 2)], Q[(1, 2) ==> my @a],
    Q[my $a <== (1, 2)], Q[(1, 2) ==> my $a] -> $source {
    my $node = $source.AST.statements[0].expression;
    is $node.^name, 'RakuAST::ApplyListInfix', 'a declaration remains a feed operand';
    is $node.infix.^name, 'RakuAST::Feed', 'the feed keeps its operator class';
    my $declaration = $node.infix.operator eq '<==' ?? $node.operands[0] !! $node.operands[1];
    is $declaration.^name, 'RakuAST::VarDeclaration::Simple', 'the feed keeps its declaration';
    is EVAL(($source ~ '; ' ~ $declaration.name ~ '.join(q[,])').AST), '1,2',
        'a declaration feed evaluates';
}

is EVAL(Q[my @a = 0; (1, 2) ==> @a; @a.join(q[,])].AST), '0,1,2',
    'a feed appends to an existing array';
is EVAL(Q[my @a; @a <== (1, 2); @a <== (3, 4); @a.join(q[,])].AST), '1,2,3,4',
    'repeated feeds preserve earlier input';
is EVAL(Q[my @a = 0; my $result = ((1, 2) ==> @a); $result.^name].AST), 'List',
    'a variable feed returns the original source type';
is EVAL(Q[my @a = 0; my $result = ((1, 2) ==> @a); $result.join(q[,])].AST), '1,2',
    'a variable feed returns only its source';
is EVAL(Q[my @a = 8; my @b; (5, 6) ==> @a ==> @b; @b.join(q[,])].AST), '5,6',
    'a tap does not send old sink elements downstream';
is EVAL(Q[my $a <== (1, 2); $a.^name].AST), 'Array', 'an undefined scalar sink becomes an Array';
dies-ok { EVAL(Q[my $a = 0; (1, 2) ==> $a].AST) }, 'a defined scalar cannot receive an append';
is EVAL(Q[my %h = a => 1; (b => 2) ==> %h; %h<a> + %h<b>].AST), 3,
    'a hash sink retains its existing entries';
is EVAL(Q[my Int @a; (1, 2) ==> @a; @a.join(q[,])].AST), '1,2',
    'a typed array accepts compatible input';
dies-ok { EVAL(Q[my Int @a; ('x', 'y') ==> @a].AST) }, 'a typed sink enforces its element type';
is EVAL(Q[my @a <== $_ for (1, 2); @a.join(q[,])].AST), '1,2',
    'a declaration modifier appends each iteration';
is EVAL(Q[my Int @a <== (1, 2) if False; @a.of.^name].AST), 'Int',
    'a false modifier retains the declaration type';
is EVAL(Q[my @log; sub source { @log.push('source'); 1, 2 }; sub argument { @log.push('argument'); 0 }; sub sink(*@data) { @log.push('sink'); @data }; my @result = (source() ==> sink(argument())); @log.join(q[,]) ~ '|' ~ @result.join(q[,])].AST),
    'argument,source,sink|0,1,2', 'source and sink arguments each evaluate once in order';
dies-ok { Q[my @a = ([],); (1, 2) ==> @a[0]].AST },
    'a subscript is outside the feed variable/call grammar';
for '==>', '<==' -> $operator {
    my $values = Q[(1, 2)].AST.statements[0].expression;
    my $declaration = Q[my @a].AST.statements[0].expression;
    my $feed = RakuAST::ApplyListInfix.new(
        infix => RakuAST::Feed.new($operator),
        operands => ($operator eq '==>' ?? ($values, $declaration) !! ($declaration, $values))
    );
    is $feed.operands.elems, 2, 'a constructed feed exposes both operands';
    is $feed.infix.operator, $operator, 'a constructed feed preserves direction';
    my $unit = RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $feed),
        Q[my @a; @a.join(q[,])].AST.statements[1]
    );
    is EVAL($unit), '1,2', 'a constructed declaration feed evaluates';
}

for
    Q[my @a; my $calls = 0; sub source { $calls++; 3, 4 }; source() ==> @a; $calls ~ '|' ~ @a.join(q[,])], '1|3,4',
    Q[my @a = 0; my @b = 9; @b <== @a <== (1, 2); @a.join(q[,]) ~ '|' ~ @b.join(q[,])], '0,1,2|9,1,2',
    Q[my @a <== grep { $_ %% 2 } <== 1..4; @a.join(q[,])], '2,4',
    Q[my @a; (1, 2) ==> @a ==> my @b; @a.join(q[,]) ~ '|' ~ @b.join(q[,])], '1,2|1,2',
    Q[(1, 2) ==> my @a for (3, 4); @a.join(q[,])], '1,2,1,2',
    Q[my Int @a <== (1, 2) unless True; @a.of.^name], 'Int',
    Q[my Int @a <== $_ if $_ > 1 for 1..3; @a.join(q[,])], '2,3',
    Q[my @a <== $_ with $_ for 1..3; @a.join(q[,])], '1,2,3',
    Q[my $i = 0; my @a <== $i while ++$i < 3; @a.join(q[,])], '1,2',
    Q[my Int @a <== 1 while False; @a.of.^name], 'Int',
    Q[my Int @a <== 1 until True; @a.of.^name], 'Int',
    Q[my @a <== $_ given (1, 2); @a.join(q[,])], '1,2',
    Q[my Int @a <== 1 with Int; @a.of.^name], 'Int',
    Q[my Int @a <== 1 without 42; @a.of.^name], 'Int',
    Q[my @a = 9; my $s = do { my @a <== $_ for 1,2; @a.join(q[,]) }; $s ~ '|' ~ @a.join(q[,])], '1,2|9',
    Q[sub f { state @a <== 1; @a.elems }; f() ~ ',' ~ f()], '1,2',
    Q[sub f { state $a <== 1; $a.elems }; f() ~ ',' ~ f()], '1,2',
    Q[my @a; (1, 2) ==> @a; my @b; @a ==> @b; @b.join(q[,])], '1,2',
    Q[my @a; my $x = [1, 2]; $x ==> @a; @a.elems ~ '|' ~ @a[0].^name], '2|Int',
    Q[my @a; my %h = a => 1; my $h = %h; $h ==> @a; @a.elems ~ '|' ~ @a[0].key], '1|a',
    Q[(a => 1) ==> my %h; %h<a>], 1
    -> $source, $expected {
    is EVAL($source), $expected, 'ordinary feed semantics: ' ~ $source;
    is EVAL($source.AST), $expected, 'AST feed semantics: ' ~ $source;
}
dies-ok { EVAL(Q[my @a <== 0..Inf].AST) }, 'an infinite feed cannot be appended eagerly';
dies-ok { EVAL(Q[my @a[2]; (1, 2) ==> @a].AST) }, 'a shaped sink cannot append';
dies-ok { EVAL(Q[my @a; @a.append(0..Inf)].AST) }, 'the common append method refuses an infinite range';
done-testing;
