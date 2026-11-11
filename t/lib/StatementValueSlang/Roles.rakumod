# Roles of the fixture slang of t/lang/parsing/slang-statement-control-value.t
# (#17460), shaped after Qwiratry's `givenroot` and `mold`: a `statement-control`
# whose action attaches a call expression, and a `routine-declarator` that builds
# a `my`-scoped method from `$*BLOCK` and reads its `meta-object`.
use nqp;
use experimental :rakuast;
unit module StatementValueSlang::Roles;

our @MOLDS;

our sub with-root(Mu $root, &block) is export {
    my $*ROOT = $root;
    block()
}

our sub root() is export { $*ROOT }

our sub kept-molds() is export { @MOLDS.List }

role Grammar is export {
    token statement-control:sym<withroot> {
        'withroot' <.end-keyword> <.ws> $<root>=<variable> <.ws> $<block>=<block>
    }
    rule keep-def($declarator) {
        :my $*BLOCK;
        <.enter-block-scope('Method')>
        $<name>=<identifier>
        'do' <.ws> $<do-block>=<blockoid>
        <.leave-block-scope>
    }
    token routine-declarator:sym<keep> {
        'keep' <.end-keyword> <keep-def=.key-origin('keep-def', 'keep')>
    }
}

role Actions is export {
    method statement-control:sym<withroot>(Mu $/) {
        self.attach: $/, RakuAST::Statement::Expression.new(
            expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('with-root'),
                args => RakuAST::ArgList.new(
                    nqp::atkey($/.hash, 'root').ast,
                    nqp::atkey($/.hash, 'block').ast)));
    }
    method routine-declarator:sym<keep>(Mu $/) {
        self.attach: $/, nqp::atkey($/.hash, 'keep-def').ast;
    }
    method keep-def(Mu $/) {
        my $name = nqp::atkey($/.hash, 'name').Str;
        my $routine := $*BLOCK;
        $routine.replace-scope('my');
        $routine.replace-name(RakuAST::Name.from-identifier('_kept_' ~ $name));
        $routine.replace-body(nqp::atkey($/.hash, 'do-block').ast);
        self.attach: $/, $routine;
        @MOLDS.push: $name => (try $routine.meta-object);
    }
}
