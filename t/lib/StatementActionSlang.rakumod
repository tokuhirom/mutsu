# Helper for t/lang/parsing/slang-statement-action-override.t: a slang whose
# actions role overrides the core `statement` action, wrapping every
# expression statement in a call that records the statement's line and text
# (the shape of Dawa's debugger stop points).
use nqp;
use experimental :rakuast;

my @seen;
sub seen-statements() is export { @seen }
sub record-statement(Int $line, Str $text) is export { @seen.push: "$line: $text"; Empty }

our role StatementGrammar { }

our role StatementActions {
    method statement(Mu $/) {
        callsame;
        my $made := $/.made;
        return unless nqp::istype($made, RakuAST::Statement::Expression);
        # Only a statement that begins a line, as Dawa's stop points.
        return unless substr($/.orig, 0, $/.from) ~~ / [ \n | ^ ] \h* $ /;
        my $text = substr($/.orig, $/.from, $/.to - $/.from).trim;
        my $line = substr($/.orig, 0, $/.from).comb("\n").elems + 1;
        my $file = $*ORIGIN-SOURCE.original-file.IO.basename;
        my $call := RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('record-statement'),
            args => RakuAST::ArgList.new(
                RakuAST::IntLiteral.new($line),
                RakuAST::StrLiteral.new("$text ($file)")));
        $/.make: RakuAST::Statement::Expression.new(
            expression => RakuAST::Circumfix::Parentheses.new(
                RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(expression => $call),
                    $made)));
    }
}

use Slangify StatementGrammar, StatementActions;
