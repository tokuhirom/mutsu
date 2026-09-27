use Test;

# In a `rule`, whitespace around the `=` of a capture alias
# (`$<k> = <.digit>+`) separates the alias from its atom; it is not
# significant. It used to become a `<.ws>`, so the alias captured the
# whitespace and the rule never matched (the EC dist's t/secp256k1.t grammar).

plan 7;

ok 'k = 12' ~~ rule { k \= $<k> = <.digit>+ }, 'alias with spaces around = in a rule';
is $<k>.join, '12', 'the alias captures each quantified atom';
ok '12' ~~ rule { $<k> = \d+ }, 'spaced alias on a backslash atom';
is ~$<k>, '12', 'captures the atom';
ok '12 34' ~~ rule { $0 = [\d+] $<y> = \d+ }, 'numbered and named aliases, still sigspace between atoms';
ok 'a = b' ~~ rule { a \= b }, 'an escaped \= stays a literal with sigspace around it';

grammar KV {
    token TOP { <pair>+ { make $<pair>».made } }
    rule pair {
        k \= $<k> = <.digit>+
        x \= $<x> = <.xdigit>+
        { make %( k => +$<k>.join, x => :16($<x>.join) ) }
    }
}
is-deeply KV.parse("k = 1\nx = 7F\n\nk = 22\nx = C6\n").made,
    [{ k => 1, x => 127 }, { k => 22, x => 198 }], 'grammar rule with spaced aliases';
