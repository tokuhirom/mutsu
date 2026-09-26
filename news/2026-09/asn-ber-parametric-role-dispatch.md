# ASN::BER parametric role dispatch

Parametric role type objects now normalize bracketed package spellings before
method punning, avoiding recursive dispatch. Construction also keeps temporary
role type-parameter bindings out of the caller's lexical environment. ASN::BER
0.7.3 now passes all four upstream test files under mutsu.
