use Test;

# X::Str::Sprintf::Directives::Unsupported should report `.directive` as
# everything after the `%` (flags, width, the Perl-5 vector flag, and the
# conversion char) and `.sequence` as `%` + that, matching Rakudo. The message
# is "Directive <directive> is not valid in sprintf format '<sequence>'".

plan 18;

sub check($fmt, $directive, $message) {
    my $ex;
    { sprintf($fmt, 1); CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Str::Sprintf::Directives::Unsupported, "$fmt throws Unsupported";
    is $ex.directive, $directive, "$fmt .directive is $directive";
    is $ex.message, $message, "$fmt message";
}

check '%q', 'q', q{Directive q is not valid in sprintf format '%q'};
check '%z', 'z', q{Directive z is not valid in sprintf format '%z'};
# Perl 5 vector flag is unsupported in Raku and makes the whole directive bad.
check '%vd', 'vd', q{Directive vd is not valid in sprintf format '%vd'};
check '%5vd', '5vd', q{Directive 5vd is not valid in sprintf format '%5vd'};
check '%v', 'v', q{Directive v is not valid in sprintf format '%v'};
# A non-ASCII conversion char must slice cleanly into .directive / .sequence.
check '%♥', '♥', q{Directive ♥ is not valid in sprintf format '%♥'};
