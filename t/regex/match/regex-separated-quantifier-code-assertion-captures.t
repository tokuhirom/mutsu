use Test;

# Net::Whois 0.0.3 validates each IPv4 octet with a code assertion inside a
# separated quantifier: `$/[*-1][*-1]` must address the accumulated octets.
plan 4;

subset IP of Str where * ~~ /^ [ (\d ** 1..3) <?{ $/[*-1][*-1] < 256 }> ] ** 4 % '.' $/;

ok '192.168.1.255' ~~ IP, 'the separated quantified capture accepts 255';
nok '192.168.1.256' ~~ IP, 'the code assertion rejects an octet above 255';
nok '192.168.1.999' ~~ IP, 'the code assertion rejects a three-digit octet above 255';
ok '192.168.1.25' ~~ IP, 'the separated quantified capture accepts a short final octet';
