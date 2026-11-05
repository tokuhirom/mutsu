# A `where` on a renamed named parameter sees the inner variable during dispatch

`multi ec(:op(@case) where @case[0] eq 'case')` evaluated its `where` before `@case` was bound, so
the clause read an undefined value (an "uninitialized value" warning) and the candidate never
matched. Candidate matching now binds the renamed inner variable to the argument first
(SQL::Abstract `expand-capture`, #16769).
