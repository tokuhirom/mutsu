use Test;

# Calling an enum VALUE is the same coercion as calling the enum type object:
# rakudo answers `Lv::TRACE` for `Lv(1)` and for `Lv::DEBUG(1)` alike, and `Nil`
# when the enum holds no such value.
#
# mutsu implemented only the type-object half, so an enum value reached through
# a variable fell through to the `CALL-ME` fallback and died with
# "No such method 'CALL-ME'". `Log::Async` hits this: its `$level` is an enum
# value whenever no CLI flag replaces it with a `* >= LEVEL` matcher, and it
# hands that to a consumer that calls it.
plan 6;

enum Lv <<:TRACE(1) DEBUG INFO>>;

my $v = DEBUG;
is $v(TRACE), TRACE, 'an enum value called with another value of its enum coerces';
is $v(2), DEBUG, '...and with the underlying numeric value';
nok $v(99).defined, '...answering an undefined value when the enum has no such value';

# The type-object form is unchanged, and the two now agree.
is Lv(1), TRACE, 'the type object still coerces';
is $v(1), Lv(1), 'value-call and type-call agree';

# The invocant is only the coercer: it does not colour the result.
my $other = INFO;
is $other(1), TRACE, 'which value is the invocant makes no difference';
