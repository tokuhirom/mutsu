# Calling a `nextcallee` continuation no longer goes through its template

A method's `nextcallee` returns a continuation (#17501): a copy of one compiled template,
`sub (|c) is raw { __mutsu_nextcallee_continue(&?ROUTINE, |c) }`, carrying the rest of the
deferral chain in its env. So a wrapper's `original(self, |c)` made three generic calls in a row:

1. the template, binding `|c`;
2. the `CallFunc` of `__mutsu_nextcallee_continue`, which went through the interpreter as a
   function fallback;
3. the candidate itself.

The value-call funnel, `vm_call_on_value_at`, now recognises a continuation by its template's body
(one pointer compare) and runs `builtin_nextcallee_continue` on the arguments directly. The
template remains for any route that does not go through the funnel.

Method::Protected wraps every `is protected` method this way. Callgrind slope (profiling build,
N 300 -> 900):

| | before | after |
| --- | ---: | ---: |
| an empty method wrapped by `my method (\|c) { my &original = nextcallee; original(self, \|c) }` | 156.8k Ir | 117.9k Ir |
| Method::Protected `is protected` `%!hash{@words.pick}++` | 272.2k Ir | 229.6k Ir |

`tests/nextcallee_continuation_skips_template.rs` pins that no `__mutsu_nextcallee_continue`
function fallback is left. `t/routines/closure/wrap-nextcallee-arguments.t` checks that the
direct call forwards the same arguments as the template did. Refs #12875.
