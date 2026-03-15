use Test;

plan 6;

grammar LiteralArgs {
    token TOP { ^ <foo(42)> $ }
    token foo($x) { $x }
}

my $literal = LiteralArgs.parse("42");
ok $literal, "token call with literal argument parses";
is ~$literal<foo>, "42", "subrule capture is preserved for literal argument call";

grammar ForwardArgs {
    token TOP($x) { ^ <foo($x)> $ }
    token foo($x) { $x }
}

my $forwarded = ForwardArgs.parse("17", :args((17,)));
ok $forwarded, "grammar parse :args feeds token parameters";
is ~$forwarded<foo>, "17", "subrule call can forward outer token parameter";

grammar StringArgs {
    token TOP { ^ <foo("ab")> $ }
    token foo($x) { $x }
}

my $string = StringArgs.parse("ab");
ok $string, "token call with string argument parses";
is ~$string<foo>, "ab", "string argument interpolates into the token body";
