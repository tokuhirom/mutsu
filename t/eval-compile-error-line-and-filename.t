use Test;

plan 8;

# `X::Comp`/`X::Syntax::*` compile-time diagnoses raised while parsing an
# EVAL'd string carry `.line` (the line WITHIN that source) and `.filename`
# (the EVAL pseudo-file, matching /EVAL/) -- exactly like every other
# X::Comp accessor rakudo exposes. Both used to be silently absent on a
# `PError::fatal_with_exception`-built exception: `.line` because the
# builder never carried a source position for `parser::parse_program`'s
# fatal branch to compute one from, and `.filename` because mutsu had no
# `.filename` accessor on exceptions at all (only the looser `.file`).

# A `=begin` with no identifier, as the whole (trimmed) source.
{
    try EVAL "=begin\n";
    is $!.^name, 'X::Syntax::Pod::BeginWithoutIdentifier',
        'right exception type for a bare =begin';
    is $!.line, 1, '.line is 1 for a single-line =begin';
    ok $!.filename ~~ /EVAL/, '.filename matches /EVAL/';
}

# Same diagnosis, but with real content spanning several MORE lines after
# the failing `=begin` -- the reported line must still be the `=begin`
# line itself, not wherever the trailing content happens to end.
{
    try EVAL "=begin\nfoo\n=end\n";
    is $!.^name, 'X::Syntax::Pod::BeginWithoutIdentifier',
        'right exception type for a multi-line =begin block';
    is $!.line, 1, '.line is still 1 with trailing content after =begin';
}

# A leading statement pushes the failing `=begin` onto a later line.
{
    try EVAL "1;\n2;\n=begin\nfoo\n=end\n";
    is $!.line, 3, '.line tracks a =begin that is not on line 1';
}

# `.filename` is not specific to this one exception class -- ANY fatal
# compile-time diagnosis with a pre-built exception object, raised while
# parsing an EVAL'd string, gets it (the backfill in `builtin_eval` is
# keyed on the error being parse-coded, not on the exception's class
# name). `X::Syntax::Malformed` (`PError::malformed`, e.g. a malformed
# initializer) is a sibling built the same way as
# `X::Syntax::Pod::BeginWithoutIdentifier`.
{
    try EVAL "my \$x = ;";
    is $!.^name, 'X::Syntax::Malformed',
        'right exception type for a malformed initializer';
    ok $!.filename ~~ /EVAL/,
        '.filename matches /EVAL/ for a sibling X::Syntax exception';
}
