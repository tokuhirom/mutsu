# HyperWhatever accepts postfix superscript powers

HyperWhatever terms can now be followed directly by Unicode superscript power
operators. Expressions such as `(**²)(1, 2, 3, 4, 5)` produce `(1 4 9 16 25)`,
and the same parsing path supports other and signed superscript exponents such
as `**³` and `**⁻¹`.

The term parser previously recognized `**` only when followed by whitespace or
a closing delimiter. It now also recognizes a valid superscript exponent as a
postfix continuation, allowing the existing postfix-power and HyperWhateverCode
compiler paths to provide the behavior without a special runtime fallback.
