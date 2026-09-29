use Test;

plan 6;

# Rakudo's NFA has no edge for a Unicode-property atom or a negated named
# class, so each is a fate that ends the declarative prefix (ADR-0111 §5); the
# longest declarative prefix then belongs to the literal branch.
is ("abx" ~~ /[ <:L>+ "x" | "ab" ]/).Str, "ab", '<:L> ends the prefix';
is ("1x" ~~ /[ <-alpha> "x" | "1" ]/).Str, "1", '<-alpha> ends the prefix';
is ("1x" ~~ /[ <-:L> "x" | "1" ]/).Str, "1", '<-:L> ends the prefix';

# Atoms that do have an NFA edge keep participating in the prefix.
is ("1x" ~~ /[ <-[a]> "x" | "1" ]/).Str, "1x", 'negated bracket class is declarative';
is ("1x" ~~ /[ \D "x" | "1" ]/).Str, "1", '\D: no match, falls to the literal';
is ("ax" ~~ /[ <alpha> "x" | "a" ]/).Str, "ax", 'positive named class is declarative';
