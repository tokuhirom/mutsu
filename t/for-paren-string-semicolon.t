use Test;

plan 4;

# A `for (...)` list whose string elements contain `;` must not be mistaken
# for the obsolete C-style `for (init; cond; step)` form. The C-style
# detector's paren scanner was quote-blind, so a semicolon inside a quoted
# string raised X::Obsolete (seen in Cro::HTTP's cookie tests iterating
# 'mycookie=raisin; SameSite=Strict' strings).

my @got;
for ("a;b", "c;d").kv -> $i, $v {
    @got.push("$i=$v");
}
is @got.join(","), "0=a;b,1=c;d", 'for (list).kv with semicolons in strings';

my @single;
for ('x; y') -> $v {
    @single.push($v);
}
is @single.join("|"), 'x; y', 'single-quoted semicolon element';

# Escaped delimiters and nesting still work.
my @nested;
for (("p;q", "r"), ("s",)) -> @pair {
    @nested.push(@pair.join("+"));
}
is @nested.join(","), "p;q+r,s", 'nested parens with semicolon string';

# The genuine C-style form is still rejected.
throws-like { EVAL 'for (my $i = 0; $i < 3; $i++) { }' }, Exception,
    message => /'C-style'/,
    'real C-style for is still X::Obsolete';
