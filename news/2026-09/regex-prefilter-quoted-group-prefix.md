# Quoted regex literals keep their literal-prefix scan prefilter

A quoted regex literal followed by another atom, such as `/ '67-8' \s $ /`,
is represented as a transparent group. The literal-prefix analysis stopped at
that group and fell back to the weaker required-inner-literal prefilter, even
though every match must begin with the quoted text.

The analysis now descends through complete transparent and capture-isolated
literal groups, while still declining capture groups, scoped `:i`/`:m` flags,
quantifiers, separators, aliases, and interpolation. A regression test pins
that the quoted prefix is retained before the trailing pattern.

Closes #8449.
