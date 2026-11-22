# A method added with `.^add_multi_method` runs its body

`.^add_multi_method` built its candidate by hand. The candidate kept the
method's invocant among its parameters, and for a declared `my method` it had
no body, because a declared routine keeps its bytecode in the routine itself,
not in the code value's `compiled_code`. A call therefore found the candidate
and answered Nil without any error.

The candidate is now built by the same code that `.^add_method` uses (now
`method_def_from_code`, split out of `methods_classhow_arms_4.rs`): the
implicit invocant is dropped and a declared method's bytecode is taken from its
routine. The only difference is that it is marked as a multi candidate (#18797).

Two smaller differences from rakudo remain and are filed as #18868:

- An added candidate is visible before `.^compose` on an already composed
  class.
- Its `Mu` invocant is not part of the ranking.
