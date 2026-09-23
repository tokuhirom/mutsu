use Test;

plan 3;

# A rule's implicit whitespace injection must preserve a native separated
# quantifier.  In particular, a bracketed whitespace separator must not be
# rewritten into an ordinary group, or the ratcheted rule stops after its first
# element and loses the real failure position.
grammar G {
    rule TOP {
        'a'+ % [ \s+ ]
        'b'
    }
}

my $parsed = G.parse("a\na\nb");
ok $parsed, 'a rule separated by whitespace parses all elements';
is $parsed.to, 5, 'the separated rule consumes the complete input';
nok G.parse("a\na\nc"), 'the failure after the separated run remains a failure';
