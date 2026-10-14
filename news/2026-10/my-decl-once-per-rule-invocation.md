# A bare `:my $x;` in a quantified regex group is declared once per rule invocation

Re-running `:my $x;` (no initializer) on a later iteration of a quantified
group no longer resets the variable: like Rakudo, it keeps the value it holds.
An initializer (`:my $x = 0;`) still re-runs. The `:my $*x;` form was already
fixed for nested rules by #13222; CSS::Module's SVG `marker` shorthand relies on
both (#13255).
