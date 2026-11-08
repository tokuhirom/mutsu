# Fixture for t/lang/parsing/slang-statementlist-wrapper.t (ADR-16888): a slang
# whose grammar role overrides the core `statementlist` method. It delegates to
# the core rule and puts a call to `note-list` in front of the statements.
use v6.e.PREVIEW;

our $count = 0;
sub note-list() is export { $count++ }
sub lists-seen() is export { $count }

role StatementListGrammar[&regular] {
    method statementlist(|) {
        my Mu $m := regular(self);
        if $m {
            my $call = RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('note-list'),
            );
            my $first = RakuAST::Statement::Expression.new(expression => $call);
            $m.make(RakuAST::StatementList.new($first, |$m.ast.statements));
        }
        $m
    }
}

sub EXPORT(|) {
    my $grammar = $*LANG.slang_grammar('MAIN');
    my $original := $grammar.HOW.find_method($grammar, 'statementlist');
    my &regular = -> Mu \cursor { $original(cursor) };
    $*LANG.define_slang(
        'MAIN',
        $grammar.^mixin(StatementListGrammar[&regular]),
        $*LANG.slang_actions('MAIN'),
    );
    %( )
}
