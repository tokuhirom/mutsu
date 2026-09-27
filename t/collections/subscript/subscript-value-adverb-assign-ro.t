use Test;

# #9811: assigning to an adverbed subscript (`@a[i]:v = x`) used to die with
# the internal "Unknown call: __mutsu_subscript_adverb". The adverb returns a
# value (or a Pair / List of values), not the element's container, so the
# assignment dies with X::Assignment::RO naming that value, as in Rakudo.

plan 12;

sub ro-message(&code) {
    code();
    CATCH { default { return .^name ~ ': ' ~ .message } }
    'lived';
}

my @t = 0, 10;
is ro-message({ @t[1]:v = 31 }),
    'X::Assignment::RO: Cannot modify an immutable Int (10)', '@a[i]:v = x';
is ro-message({ (@t[1]:v) = 31 }),
    'X::Assignment::RO: Cannot modify an immutable Int (10)', '(@a[i]:v) = x';
is ro-message({ my $r = (@t[1]:v = 31) }),
    'X::Assignment::RO: Cannot modify an immutable Int (10)', 'assignment in expression position';
is ro-message({ @t[1]:k = 31 }),
    'X::Assignment::RO: Cannot modify an immutable Int (1)', '@a[i]:k = x';
is ro-message({ @t[1]:kv = 31 }),
    'X::Assignment::RO: Cannot modify an immutable Int (1)', '@a[i]:kv = x names the key';
is ro-message({ @t[1]:p = 31 }),
    'X::Assignment::RO: Cannot modify an immutable Pair (1 => 10)', '@a[i]:p = x';
is-deeply @t, [0, 10], 'array untouched';

my %h = a => 1;
is ro-message({ %h<a>:v = 3 }),
    'X::Assignment::RO: Cannot modify an immutable Int (1)', '%h<k>:v = x';
is ro-message({ %h{"a"}:!v = 3 }),
    'X::Assignment::RO: Cannot modify an immutable Int (1)', '%h{k}:!v = x';

# An Array element is itself a container: `:v` hands it back and the
# assignment stores into it.
my @n = 0, [1, 2];
@n[1]:v = 31;
is-deeply @n, [0, [31]], ':v on an Array element list-assigns into it';

# The same rule for any non-container expression value.
my $x = 10;
is ro-message({ (1 + $x) = 3 }),
    'X::Assignment::RO: Cannot modify an immutable Int (11)', '(EXPR) = x names the value';
is ro-message({ (1, 2) = 3 }),
    'X::Assignment::RO: Cannot modify an immutable Int (1)', '(1, 2) = x names the first element';
