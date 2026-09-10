use Test;

plan 2;

# An enum whose explicit Int value reaches i64::MAX must not abort the
# interpreter when the running auto-increment counter passes MAX.
# (Regression: panicked with "attempt to add with overflow". Reaching these
#  assertions at all proves the declaration did not abort.)
enum BigE (Top => 9223372036854775807, Other => 0);
is Top.value, 9223372036854775807, 'explicit max enum value preserved';
is Other.value, 0, 'following explicit value after max counter is fine';
