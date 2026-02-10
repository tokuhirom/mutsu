use Test;
plan 4;

ok defined getlogin(), "getlogin returns a value";
ok gethost()["name"], "gethost returns hostname";
ok gethost("localhost")["addr"], "localhost resolves to address";
ok gethost("127.0.0.1")["addrs"][0], "loopback address available";
