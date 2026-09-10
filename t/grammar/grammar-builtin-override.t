use Test;

plan 6;

# Grammar tokens with names that shadow built-in character classes
# should take precedence over the built-in when used as subrules.

# Test: grammar defines its own "ident" token (shadows builtin <ident>)
grammar G {
    token ident { <+ graph - punct> <+ graph - [\<\>\{\}\[\]&=%$]>* }
    token name { <ident> }
}

# Direct call should work
my $r1 = G.parse("bar-x", :rule<ident>);
ok $r1.defined, "direct call to custom ident matches";
is ~$r1, "bar-x", "direct ident matches full 'bar-x'";

# As subrule should also work (grammar's ident, not builtin)
my $r2 = G.subparse("bar-x", :rule<name>);
ok $r2.defined, "subrule call to custom ident matches";
is ~$r2, "bar-x", "subrule ident matches full 'bar-x'";

# Verify the built-in ident still works in non-grammar context
ok "foo123" ~~ /<ident>/, "builtin <ident> still works outside grammar";
is $/.Str, "foo123", "builtin <ident> matches correctly";
