use Test;

# From the Net::Whois::Async ecosystem dist: a class-scoped `subset` whose
# `where` regex has a `<?{ $/[*-1][*-1] < 256 }>` assertion. The class body
# exit used to persist `$/` / `$0` as class "statics", so the assertion read
# a stale Nil instead of the live match state.

plan 6;

class C {
    subset IP of Str where * ~~ /^ [ (\d ** 1..3) <?{ $/[*-1][*-1] < 256 }> ] ** 4 % '.' $/;
    subset Cap of Str where { "1" ~~ /(\d) <?{ $0 eq '1' && $/.defined }>/ };
}

ok '93.184.216.34' ~~ C::IP, 'valid octets match the subset';
nok '999.0.0.1' ~~ C::IP, 'octet over 255 is rejected';
nok '256.0.0.1' ~~ C::IP, '256 is rejected';
nok '1.2.3' ~~ C::IP, 'three octets are rejected';
ok 'x' ~~ C::Cap, '$0 and $/ are live inside the assertion';

grammar G {
    subset Num of Str where { "7" ~~ /(\d) <?{ $0 == 7 }>/ };
}
ok 'x' ~~ G::Num, 'same inside a grammar package';

done-testing;
