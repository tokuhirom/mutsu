# Roles of the fixture slang of t/lang/parsing/slang-declarator-dynvar-nested-hook.t
# (#16888, the shape of EEL 0.3's `pattern`): a `routine-declarator` whose
# `:my $*TALLY` the statement-list action of the declared method's body sets,
# where that body calls the core `arglist` and `circumfix:sym<( )>` (so a
# parenthesized argument is a production nested inside the body's).
use nqp;
use experimental :rakuast;
unit module DeclaratorDynvarSlang::Roles;

role Tallied is export { has $.tally is rw }

role Grammar[Mu $regular, Mu $regular-arglist, Mu $regular-paren] is export {
    token routine-declarator:sym<tally> {
        :my $*TALLY-ON = True;
        :my $*TALLY;
        'tally' <.end-keyword> <method-def=.key-origin('method-def', 'method')>
    }
    method statementlist(|) {
        if (try $*TALLY-ON) && (try $*BLOCK ~~ RakuAST::Method) { self.tally-body }
        else { $regular.(self) }
    }
    token tally-body {
        :my $*TALLY-ON = False;
        <.ws> $<word>=[\w+] '(' <arglist> ')' <.ws>
    }
    method arglist(|args) { $regular-arglist.(self, |args) }
    method circumfix:sym<( )>(|args) { $regular-paren.(self, |args) }
}

role Actions is export {
    method routine-declarator:sym<tally>(Mu $/) {
        my $method = nqp::atkey($/.hash, 'method-def').ast;
        my $routine = $method.meta-object;
        $routine does Tallied;
        $routine.tally = $*TALLY;
        self.DECLARATOR($/, $method);
    }
    method tally-body(Mu $/) {
        my $word = nqp::atkey($/.hash, 'word').Str;
        $*TALLY = $word ~ '/' ~ nqp::atkey($/.hash, 'arglist').ast.args.elems;
        self.attach($/, RakuAST::StatementList.new(RakuAST::Statement::Expression.new(
            expression => RakuAST::Literal.from-value($word))));
    }
}
