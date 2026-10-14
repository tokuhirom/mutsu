use Test;

# `Int.Str(:superscript, :subscript)` is a named row of the method table
# (ADR-11276 §9.104); the other `Str` candidates ignore a named argument
# they do not know.

plan 11;

is 42.Str(:superscript), '⁴²', ':superscript';
is 42.Str(:subscript), '₄₂', ':subscript';
is (-7).Str(:superscript), '⁻⁷', 'a negative Int raises its sign';
is (2**70).Str(:subscript), '₁₁₈₀₅₉₁₆₂₀₇₁₇₄₁₁₃₀₃₄₂₄', 'a big Int';
is 42.Str(:superscript, :subscript), '⁴²', ':superscript wins over :subscript';
is 42.Str(:!superscript), '42', 'a false :superscript renders plainly';
is 42.Str(:foo), '42', 'an unknown named argument is ignored';
is 1e3.Str(:superscript), '1000', 'a Num ignores :superscript';
is 3.5.Str(:subscript), '3.5', 'a Rat ignores :subscript';
is True.Str(:superscript), 'True', 'a Bool renders its name';
is "12".Str(:superscript), '12', 'a Str is itself';
