use Test;

# Under `use fatal`, an unhandled Failure produced while building a
# list/array/hash composite literal must explode immediately, before the
# composite is ever bound to a variable -- matching real Raku. mutsu's
# per-assignment `fatal_mode` checks only ever saw the composite's own
# value (an Array/Hash, never itself a Failure instance), so a Failure
# nested inside one of the composite's elements slipped through silently.
# See todo/tickets/fatal-mode-does-not-explode-failure-nested-in-list-literal.md.

plan 7;

# 1. The ticket's minimal repro: a Failure as one element of a parenthesized
#    list literal assigned to an array must explode at construction time.
sub list_literal_element_failure() {
    use fatal;
    my @a = (1, "a".Int, 3);
    return "unreached";
}
dies-ok { list_literal_element_failure() },
    "a Failure nested in a list-literal element explodes under use fatal";

# 2. Hash literal analog (`my %h = (key => "a".Int, ...)`).
sub hash_literal_element_failure() {
    use fatal;
    my %h = (a => 1, b => "a".Int, c => 3);
    return "unreached";
}
dies-ok { hash_literal_element_failure() },
    "a Failure nested in a hash-literal element explodes under use fatal";

# 3. Explicit `%(...)` hash-composer syntax.
sub hash_composer_element_failure() {
    use fatal;
    my $h = %(a => 1, b => "a".Int, c => 3);
    return "unreached";
}
dies-ok { hash_composer_element_failure() },
    "a Failure nested in a %(...) hash-composer element explodes under use fatal";

# 4. Nested list: the Failure is inside an inner bracket array, itself an
#    element of the outer array literal.
sub nested_list_element_failure() {
    use fatal;
    my @a = [1, ["a".Int], 3];
    return "unreached";
}
dies-ok { nested_list_element_failure() },
    "a Failure nested inside an inner array literal explodes under use fatal";

# 5. Sanity: without `use fatal`, the same list literal builds fine and
#    the Failure element survives as a soft value.
sub list_literal_without_fatal() {
    my @a = (1, "a".Int, 3);
    return @a.elems;
}
is list_literal_without_fatal(), 3,
    "without use fatal, a list literal keeps a Failure element as a soft value";

# 6. Should NOT explode: a Failure that is caught and replaced by `try`
#    before being embedded in the composite. The `try` block itself runs
#    under implicit fatal mode and throws internally, but the surrounding
#    `//` substitutes a non-Failure value, so nothing unhandled ever
#    reaches the outer list literal.
sub list_literal_handled_via_try() {
    use fatal;
    my @a = (1, ((try { "a".Int }) // 99), 3);
    return @a.join(",");
}
is list_literal_handled_via_try(), "1,99,3",
    "a Failure caught by try before being embedded does not explode the composite";

# 7. Should NOT explode: an ordinary composite literal with no Failure at
#    all still builds normally under use fatal (the fast, non-fatal-hit
#    path through the new composite check).
sub plain_list_literal_under_fatal() {
    use fatal;
    my @a = (1, 2, 3);
    return @a.join(",");
}
is plain_list_literal_under_fatal(), "1,2,3",
    "an ordinary composite literal with no Failure builds normally under use fatal";
