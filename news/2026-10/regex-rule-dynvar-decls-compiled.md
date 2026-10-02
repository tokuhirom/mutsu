# Grammars with `:my $*x` rule declarations run on the compiled regex engine

A grammar with even one rule that declared a dynamic variable (`token r { :my $*x = …; … }`) used to
send every match and every subrule call of a parse to the regex tree walk. Such a grammar now runs
on the compiled engine (ADR-0135 Slice E, ninth part).

When a rule is called, its declarations are initialized once and joined to the call's binding window.
They are removed when the rule returns or fails, and installed again when backtracking resumes inside
the rule.

This also fixes a wrong result, rakudo being the reference. In such grammars a ratcheted `token`
called as a subrule was re-entered for its shorter ends; it now commits to its first end, as it does
in any other grammar. Across `t/grammar`, `t/regex` and `t/modules`, the number of matches the walk
answered whole fell from 4,537 to 3,352.
