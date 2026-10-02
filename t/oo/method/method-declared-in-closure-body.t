use Test;

# A `method` declared inside a closure in a class -- a pointy block, an
# anonymous sub, a bare block, `try`, `gather`, `do` -- is installed in the
# class, closing over the closure's latest run (#10820).

plan 7;

class Foo {
    method o2 { my $c = -> { method pm { "pm" } }; 1 }
    method o3($n) { my &f = sub { method qm { "qm $n" } }; f(); 1 }
    method mk { for 1, 2 -> $k { my $c = -> { method km { "km $k" } }; $c() }; 1 }
    method tr { try { method tm { "tm" } }; 1 }
    method g { my @a = gather { take 1; method gm { "gm" } }; 1 }
    has $.x = { method rm { "rm" } };
    my $y = do { method dm { "dm" } };
}

is Foo.pm, 'pm', 'a pointy block';
Foo.o3(4);
is Foo.qm, 'qm 4', 'an anonymous sub, closing over its routine';
Foo.mk;
is Foo.km, 'km 2', "closing over the closure's latest run";
is Foo.tm, 'tm', 'a try block';
is Foo.gm, 'gm', 'a gather block';
is Foo.rm, 'rm', 'an attribute default block';
is Foo.dm, 'dm', 'a do block in the class body';
