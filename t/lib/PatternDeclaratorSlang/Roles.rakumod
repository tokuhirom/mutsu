# Roles of the fixture slang of t/lang/parsing/slang-statementlist-pattern-declarator.t
# (#16888, the shape of EEL 0.3's `pattern` declarator): a `routine-declarator`
# that reads a method with the core `method-def`, whose body the grammar's own
# `statementlist` parses in the slang's language while `$*BLOCK` is that method.
use nqp;
use experimental :rakuast;
unit module PatternDeclaratorSlang::Roles;

role Tagged is export { has $.tag is rw }

class Shouted is export {
    has $.word;
    method shout { $!word.uc ~ '!' }
}

role Grammar[Mu $regular] is export {
    token routine-declarator:sym<shout> {
        :my $*SHOUT = True;
        :my $*SHOUT-TAG;
        'shout' <.end-keyword> <method-def=.key-origin('method-def', 'method')>
    }
    # Only the declared method's own body is in the slang's language.
    method statementlist(|) {
        if (try $*SHOUT) && (try $*BLOCK ~~ RakuAST::Method) { self.shout-body }
        else { $regular.(self) }
    }
    token shout-body {
        :my $*SHOUT = False;
        <.ws> $<word>=[\w+] <.ws>
    }
}

role Actions is export {
    method routine-declarator:sym<shout>(Mu $/) {
        my $method = nqp::atkey($/.hash, 'method-def').ast;
        my $routine = $method.meta-object;
        $routine does Tagged;
        $routine.tag = $*SHOUT-TAG;
        self.DECLARATOR($/, $method);
    }
    method shout-body(Mu $/) {
        my $word = nqp::atkey($/.hash, 'word').Str;
        # A compile-time error the action reports (`HLL::Grammar.panic`).
        $/.panic("cannot shout $word") if $word eq 'panic';
        # The core `make`: the importer's `&make` from `EXPORT` is not this
        # module's.
        make $word;
        $*SHOUT-TAG = $*BLOCK.name.canonicalize ~ '/'
            ~ ($*BLOCK.signature ?? $*BLOCK.signature.parameters.elems !! 'none')
            ~ '/' ~ $/.made;
        self.attach($/, RakuAST::StatementList.new(RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyPostfix.new(
                operand => RakuAST::Literal.from-value(Shouted.new(:$word)),
                postfix => RakuAST::Call::Method.new(
                    name => RakuAST::Name.from-identifier('shout'))))));
    }
}
