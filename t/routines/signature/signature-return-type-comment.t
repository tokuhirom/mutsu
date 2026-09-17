use Test;

plan 4;

# A `#`-comment trailing the return type, before the closing `)`, must not
# become part of the parsed type name (Date::Utils ecosystem distribution:
# `--> UInt # range 0..6`). Before this fix the comment text leaked into the
# return-type spec ("UInt # range 0..6"), which `is_definite_return_spec`
# then misclassified as a definite-return-VALUE spec rather than the type
# `UInt` -- rejecting any `return $x` in the body with a spurious
# "No return arguments allowed" compile error.
sub day-index(
    Int $n, # a plain positional param comment
    --> UInt # range 0..6
) {
    return $n + 1;
}
is day-index(2), 3, 'return with a trailing #-comment on the return type works';

is &day-index.signature.returns.^name, 'UInt',
    'the comment does not leak into the parsed return type name';

# Same shape but the comment sits right before a `;` multi-invocant separator.
sub with-semicolon(
    Int $n; # separator comment
    --> Int # another comment
) {
    return $n * 2;
}
is with-semicolon(5), 10, 'return works with a comment before the return type too';

# A comment on its own with no following whitespace quirks.
sub plain-comment(--> Str #`( embedded comment )) {
    return "ok";
}
is plain-comment(), 'ok', 'an embedded #`(...) comment after the return type is also skipped';
