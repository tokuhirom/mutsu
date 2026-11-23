use Test;

# A language-version pragma is taken only as the unit's first statement
# (comments and Pod aside); anywhere else it is X::Language::TooLate. The dot
# after `v6` is optional.

plan 9;

sub version-of(Str $code) {
    my $proc = run($*EXECUTABLE, '-e', $code ~ 'print $*RAKU.version', :out, :err);
    $proc.err.slurp(:close);
    $proc.out.slurp(:close)
}

is version-of('use v6c; '), '6.c', 'use v6c is use v6.c';
is version-of('use v6.c; '), '6.c', 'use v6.c';
is version-of("# a comment\n=begin pod\n\nP\n\n=end pod\nuse v6.c; "), '6.c',
    'comments and Pod may come first';

for 'say 1; use v6.c;', 'use Test; use v6;', '{ use v6.d; }', 'BEGIN { 1 }; use v6.c;' -> $code {
    throws-like { EVAL $code }, X::Language::TooLate, "too late: $code";
}

{
    my $out = run($*EXECUTABLE, '-MTest', '-e', 'use v6.c; print $*RAKU.version', :out, :err);
    is $out.out.slurp(:close), '6.c', '-MModule keeps a leading version pragma first';
    $out.err.slurp(:close);
}

lives-ok { EVAL 'use v6.d; 1' }, 'a leading pragma in an EVAL';
