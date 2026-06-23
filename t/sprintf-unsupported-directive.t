use Test;

# X::Str::Sprintf::Directives::Unsupported should report `.directive` as
# everything after the `%` (flags, width, the Perl-5 vector flag, and the
# conversion char), and `.sequence` (plus the message's format slot) as the
# FULL original format string, matching Rakudo. The message is
# "Directive <directive> is not valid in sprintf format '<sequence>'".

plan 21;

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

# An invalid directive embedded in surrounding literal text must report the
# WHOLE format string in the message's format slot and in `.sequence` (not just
# the offending `%directive`). (Rakudo's `.directive` token for an embedded bad
# directive is computed inconsistently from an internal cursor, so we only pin
# down the `.sequence` / format-slot here, which is the full original format.)
{
    my $ex;
    { sprintf('a%qb', 1); CATCH { default { $ex = $_ } } }
    is $ex.sequence, 'a%qb', 'embedded bad directive .sequence is the full format';
    ok $ex.message.ends-with(q{in sprintf format 'a%qb'}),
        'embedded bad directive reports the full format in the message';
    is $ex.directive, 'q', 'embedded bad directive .directive is the local span';
}
