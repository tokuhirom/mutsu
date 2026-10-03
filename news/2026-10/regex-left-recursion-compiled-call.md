# Left-recursive grammar rules run from the compiled regex engine

A left-recursive rule such as `token expr { <expr> '+' <term> | <term> }` used to send every call
of it to the regex tree walk's eager producer. So did any call made while such a rule was being
evaluated. The compiled engine now evaluates these calls itself (ADR-0135 Slice E, thirteenth
part).

It calls mutsu's growing-seed loop directly. That loop moved out of the walk into its own module, so
the walk and the compiled engine share one implementation. The results are unchanged. Across
`t/grammar`, `t/regex` and `t/modules`, 569 walk bridges are gone.
