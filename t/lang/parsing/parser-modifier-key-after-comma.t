use Test;

plan 30;

# BDD::Behave::Runner builds a multiline record with a `given` pair key.
# Modifier keywords after a comma are ordinary keys when followed by =>.
for <if unless with without for while until given when> -> $key {
    my $pair = $key ~ ' => 42';
    my @parenthesized = EVAL '(1, ' ~ $pair ~ ')';
    is @parenthesized[1].key, $key, "$key is a parenthesized list pair key";
    is @parenthesized[1].value, 42, "$key keeps its pair value";
    my @bracketed = EVAL '[1, ' ~ $pair ~ ']';
    is @bracketed[1].key, $key, "$key is a bracketed list pair key";
}

my %record = (
    negated => False,
    given => 'actual',
    expected => 'wanted',
);
is %record<given>, 'actual', 'multiline record keeps the given key';
is-deeply (1, 2 if True), (1, 2), 'conditional list modifier still applies';
is-deeply (1, 2, if True), (1, 2), 'modifier after a trailing comma still applies';
