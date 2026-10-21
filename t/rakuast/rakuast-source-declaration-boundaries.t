use v6;
use MONKEY-SEE-NO-EVAL;
use MONKEY-TYPING;
use experimental :rakuast;
use Test;

plan 11;

my $doc = Q[DOC INIT { die 'unexpected DOC execution' }; 42].AST;
ok $doc.raku.contains('RakuAST::Term::Name'),
    'a DOC phaser exposes the ordinary Nil statement';
nok $doc.raku.contains('source-doc-phaser'),
    'DOC source provenance is absent from the public constructor';
is EVAL($doc), 42, 'DOC source provenance survives lowering without executing the phaser';

my $alternates = Q[
    class Left { }
    class Right { }
    multi sub choose (Left $x) | (Right $x) { $x.^name }
    (choose(Left.new), choose(Right.new)).join(',')
].AST;
nok $alternates.raku.contains('source-signature-alternates'),
    'alternate signatures do not invent a public routine field';
is EVAL($alternates), 'Left,Right', 'alternate signatures dispatch after AST lowering';

my $supersede = Q[
    {
        use MONKEY-TYPING;
        sub choice { 1 }
        supersede sub choice { 2 }
        choice()
    }
].AST;
nok $supersede.raku.contains('source-supersede'),
    'supersede provenance is absent from the public constructor';
ok $supersede.raku.contains('RakuAST::Sub.new'),
    'supersede remains a public sub declaration';

my $composition = Q[(* o (* * 2))(* + 1)(3)].AST;
ok $composition.raku.contains('RakuAST::Infix.new("o")'),
    'a composition with bare Whatever keeps its written infix';
is EVAL($composition), 7, 'the written composition lowers to its executable closure';

my $augment = Q[
    role Existing { }
    use MONKEY-TYPING;
    augment role Existing { }
].AST;
ok $augment.raku.contains('RakuAST::Role.new'),
    'a role augmentation retains its public package kind';
dies-ok { EVAL($augment) },
    'a role augmentation still raises its declaration error after lowering';
