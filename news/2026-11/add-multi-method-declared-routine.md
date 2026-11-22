# A method added with `.^add_multi_method` runs its body

`.^add_multi_method` built its candidate by hand, and for a declared
`my method` the candidate had no body, because a declared routine keeps its bytecode in the routine itself,
not in the code value's `compiled_code`. A call therefore found the candidate
and answered Nil without any error.

The candidate is now built by the same code that `.^add_method` uses (now
`method_def_from_code`, split out of `methods_classhow_arms_4.rs`), so a
declared method's bytecode is taken from its routine. Unlike a single method,
a multi candidate keeps its invocant parameter, because the invocant's type and
smiley (`(Mu:U:)` vs `(Mu:D:)`) help choose the candidate (#18797).

Two smaller differences from rakudo remain and are filed as #18868:

- An added candidate is visible before `.^compose` on an already composed
  class.
- Its `Mu` invocant is not part of the ranking.
