use Test;

# An attribute is installed when the class body is composed, so a *runtime*
# statement modifier cannot gate the declaration. rakudo keeps the attribute,
# its traits and its default whatever the condition says, and only evaluates
# the condition itself, once, as the class body runs. mutsu rejected every
# spelling outright ("Confused. Two terms in a row"), which is what blocked
# Test::Declare's `has $.class is required when !*.DEFINITE;`.
#
# This pins rakudo's behaviour, not mutsu's: every assertion below is green
# under rakudo itself.

plan 8;

# `when` — the spelling Test::Declare writes.
class Callable-ish {
    has $.class is required when !*.DEFINITE;
    has Str $.method is required;
}
is Callable-ish.new(class => Int, method => 'new').class, Int,
    'an attribute declared under a `when` modifier exists';
dies-ok { Callable-ish.new(method => 'new') },
    'its `is required` trait still applies, whatever the condition says';

# `if` with a false condition: the declaration and its default both survive.
class False-Cond {
    has $.x = 5 if 0;
}
is False-Cond.new.x, 5, 'a false `if` gates neither the attribute nor its default';

# `unless` likewise.
class Unless-Cond {
    has $.y = 7 unless 1;
}
is Unless-Cond.new.y, 7, 'a true `unless` gates neither the attribute nor its default';

# The condition IS evaluated, once, while the class body runs.
my $runs = 0;
class Side-Effect {
    has $.z = 3 if $runs++;
}
is $runs, 1, 'the modifier condition runs exactly once, as the class body is composed';
is Side-Effect.new.z, 3, 'and the attribute keeps its default regardless';

# A private attribute takes one too.
class Private-Attr {
    has $!p = 11 if 0;
    method peek { $!p }
}
is Private-Attr.new.peek, 11, 'a private attribute declaration takes a modifier as well';

# A plain declaration with no modifier is unchanged.
class Plain {
    has $.q = 2;
}
is Plain.new.q, 2, 'an unmodified declaration still works';
