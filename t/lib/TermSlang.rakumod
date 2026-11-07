# Fixture for t/lang/parsing/slang-term-hook.t (ADR-15762 slice 2): a slang whose
# grammar role adds a `term:sym<...>` and a `number:sym<...>` production, each with
# an action that attaches the RakuAST of the value it stands for.
use v6.e.PREVIEW;

our role TermGrammar {
    token term:sym<answer> { 'answer' <.end-keyword> }
    token number:sym<roman> { 'roman:' $<digits>=<[IVXLCDM]>+ }
}

our role TermActions {
    method term:sym<answer>(Mu $/) {
        self.attach: $/, RakuAST::IntLiteral.new(42);
    }
    method number:sym<roman>(Mu $/) {
        my %value = :I(1), :V(5), :X(10), :L(50), :C(100), :D(500), :M(1000);
        my @digits = $<digits>.Str.comb.map({ %value{$_} });
        my $total = 0;
        for @digits.kv -> $i, $d {
            $total += (@digits[$i + 1] // 0) > $d ?? -$d !! $d;
        }
        self.attach: $/, RakuAST::IntLiteral.new($total);
    }
}

use Slangify TermGrammar, TermActions;
