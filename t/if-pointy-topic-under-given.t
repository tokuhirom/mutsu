use Test;

# `if/with EXPR -> $_ { }` binds a FRESH lexical `$_` (like `for -> $_`), so its
# topic must NOT flow back to an enclosing `given $x`'s source variable. mutsu
# lowered the pointy to `my $_ = EXPR`, whose write hit the given-topic writeback
# and clobbered the source (a `given %h<type> { when ... { if COND -> $_ {} } }`
# overwrote `%h<type>` with COND — Template::Mustache's section handler relies on
# the hunk's `<type>` surviving `elsif ... $datum -> $_`).

plan 7;

# Scalar given topic.
my $s = 'keep';
given $s { if [1, 2] -> $_ { } }
is $s, 'keep', "if -> \$_ does not clobber a scalar given topic";

# `with` form. (The `with EXPR -> $_` desugar emits `my $_ = tmp` in the branch
# via a separate parser path that still clobbers — tracked separately.)
my $t = 'keep2';
given $t { with [3, 4] -> $_ { } }
todo 'with EXPR -> $_ desugar still clobbers the given topic';
is $t, 'keep2', "with -> \$_ does not clobber the given topic";

# Hash-element given topic (the Mustache shape).
my %h = type => 'section', inverted => False;
given %h<type> {
    when 'section' {
        if !%h<inverted> and [9, 9] -> $_ { }
    }
}
is %h<type>, 'section', "if -> \$_ does not clobber a hash-element given topic";

# elsif chain — the pointy is on a later arm.
my %g = type => 'section', a => False, b => False, c => True;
my $val = 'result';
given %g<type> {
    when 'section' {
        $val = do {
            if %g<a> { 'x' }
            elsif %g<b> { 'y' }
            elsif %g<c> -> $_ { 'z' }
            else { 'w' }
        };
    }
}
is %g<type>, 'section', "elsif -> \$_ leaves the given topic intact";
is $val, 'z', "the pointy elsif arm still runs and yields its value";

# The bound `$_` inside the block IS the condition value.
my $seen;
given 'topic' { if [7, 8] -> $_ { $seen = $_ } }
is-deeply $seen, [7, 8], "\$_ inside the pointy block is the condition value";

# A `when`-succeed inside the pointy block unwinds past the pointy scope's
# restore; the enclosing `given`'s element writeback must still flush the given's
# OWN topic, not the leftover pointy `$_`. (Template::Mustache's nested `{{#.}}`
# implicit iterator hit this — the shared section hunk's `<type>` was overwritten
# with the iterated datum.)
my %w = type => 'section', inverted => False;
given %w<type> {
    when 'section' {
        if !%w<inverted> and [5, 6] -> $_ {
            when Iterable { 'iter' }
            default { 'def' }
        }
    }
}
is %w<type>, 'section',
    "a when-succeed inside a pointy block leaves the given topic intact";
