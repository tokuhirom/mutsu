# Fixture for t/lang/parsing/slang-scoped-declarator.t (ADR-15762, #13370): the
# shape of Qwiratry's `aggsub`. A `routine-declarator` production opens a method
# scope (`enter-block-scope`), reads an identifier, type names, a signature,
# colonpairs and a `blockoid`, and its action reads the body back with DEPARSE.
use v6.e.PREVIEW;
use nqp;

my %SEEN;
sub scoped-seen(Str $name) is export { %SEEN{$name} }
sub scoped-record(*@f) is export {
    %SEEN{@f[0]} = %(
        types => @f[1], signature => @f[2], pairs => @f[3],
        body => @f[4], body-class => @f[5], order => @f[6],
    );
}

my role ScopeGrammar {
    token routine-declarator:sym<scoped> {
        'scoped' <.end-keyword>
        <scoped-def=.key-origin('scoped-def', 'scoped')>
    }

    rule scoped-def($declarator) {
        :my $*BLOCK;
        <.enter-block-scope('Method')>
        $<name>=<.identifier>
        { $*R.declare-lexical($/.Nodify('VarDeclaration::Implicit::Self').new) }
        [ '[' <.ws> $<type-args>=<typename>+ % [<.ws> ',' <.ws>] <.ws> ']' ]?
        [ '(' <signature(:ON-ROUTINE(1))> ')' ]?
        [ <colonpair> ]*
        { if $<signature> { $*BLOCK.replace-signature($<signature>.ast); } }
        { $*R.create-scope-implicits(); }
        $<body>=<blockoid>
        <.leave-block-scope>
    }

    token statement-prefix:sym<NOTED> { 'NOTED' <.kok> <blorst> }
}

my role ScopeActions {
    method routine-declarator:sym<scoped>(Mu $/) {
        self.attach: $/, $<scoped-def>.ast;
    }

    method statement-prefix:sym<NOTED>(Mu $/) {
        self.attach: $/, RakuAST::Term::Name.new(RakuAST::Name.from-identifier('Nil'));
    }

    method scoped-def(Mu $/) {
        my @types = $<type-args> ?? $<type-args>.list.map(~*) !! ();
        my @fields =
            ~$<name>,
            @types.join(','),
            ($<signature>.defined ?? ~$<signature> !! ''),
            ($<colonpair> ?? $<colonpair>.list.map(~*).join(' ') !! ''),
            $<body>.ast.DEPARSE,
            $<body>.ast.^name,
            EVAL('Same').raku;
        # The action runs in the slang host, so what it learned travels in the
        # node it attaches: a call the program runs.
        my $call = RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('scoped-record'),
            args => RakuAST::ArgList.new(
                |@fields.map({ RakuAST::StrLiteral.new($_) })
            ),
        );
        self.attach: $/, $call;
    }
}

use Slangify ScopeGrammar, ScopeActions;
