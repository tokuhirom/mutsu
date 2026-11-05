use Test;

# X::Str::Sprintf::Directives::Unsupported should report `.directive` as
# everything after the `%` (flags, width, the Perl-5 vector flag, and the
# conversion char), and `.sequence` (plus the message's format slot) as the
# FULL original format string, matching Rakudo. The message is
# "Directive <directive> is not valid in sprintf format '<sequence>'".

plan 26;

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

# An invalid directive embedded in surrounding literal text: rakudo's
# `.directive` is the format string without its first character (`%qb` for
# `a%qb`), and `.sequence` / the message's format slot are the full format.
{
    my $ex;
    { sprintf('a%qb', 1); CATCH { default { $ex = $_ } } }
    is $ex.sequence, 'a%qb', 'embedded bad directive .sequence is the full format';
    ok $ex.message.ends-with(q{in sprintf format 'a%qb'}),
        'embedded bad directive reports the full format in the message';
    is $ex.directive, '%qb', 'embedded bad directive .directive is the format minus its first char';
}

# A format with trailing text reports all of it as the directive, and a message
# longer than 71 columns wraps at a space.
check '%ld a big', 'ld a big', q{Directive ld a big is not valid in sprintf format '%ld a big'};
{
    my $ex;
    { sprintf('%ld a big number', 1); CATCH { default { $ex = $_ } } }
    is $ex.directive, 'ld a big number', '.directive keeps the trailing text';
    is $ex.message,
        "Directive ld a big number is not valid in sprintf format '%ld a big\nnumber'",
        'a long message wraps at 71 columns';
}
