use Test;
use MONKEY-SEE-NO-EVAL;

# A punctuation-only name in parentheses is only a prefix operator when one has
# been DECLARED with that name. The parser used to take the spelling alone as
# proof: any `(<punctuation>)` followed by whitespace was consumed as a call to
# `prefix:<(...)>` and the rest of the expression was demanded as its operand.
#
# That shadowed Raku's own `(*)` -- a parenthesised `Whatever` -- everywhere a
# space followed it. `(*)` at end of input happened to work (no whitespace),
# `(* )` and `( *)` worked (the spelling no longer matched), and `(*) 1` worked
# (the "operand" was there), so the gap only showed up as an unexplained parse
# failure in real code: `method shape(::?ROLE:D:) { (*,) }` in Array::Agnostic,
# and the same shape in the distributions surveyed for issue #7988.
#
# rakudo parses every line below.

plan 7;

is (*).WHAT.^name, 'Whatever', 'a parenthesised Whatever is a term, not an operator call';

my $spaced = (*) ;
is $spaced.WHAT.^name, 'Whatever', 'whitespace after it does not make it an operator';

sub returns-whatever-list() { (*,) }
is returns-whatever-list().elems, 1, '(*,) as a block final statement parses';
is returns-whatever-list()[0].WHAT.^name, 'Whatever', 'and holds the Whatever itself';

sub returns-whatever() { (*) }
is returns-whatever().WHAT.^name, 'Whatever', '(*) as a block final statement parses';

# `(**)` is the same shape with HyperWhatever.
sub returns-hyper() { (**) }
is returns-hyper().WHAT.^name, 'HyperWhatever', '(**) as a block final statement parses';

# A prefix operator that really is declared still wins over the term reading.
{
    sub prefix:<(+-)> ($thing) { "ABOUT$thing" }
    is EVAL(q[ (+-) "fish" ]), 'ABOUTfish', 'a declared parenthesised prefix operator still applies';
}
