# Multi method `where` clauses resolve in the declaring module

Candidate matching for a multi method evaluated its `where` clauses in the caller's compilation
unit, so a role or class the module itself declared (and the caller never imported) died with
"Undeclared name". Matching now runs in the method's declaring unit, as multi subs already did.
Found through the ASN::BER distribution, whose `t/03-long-integers.t` now passes.
