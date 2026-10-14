use Test;

plan 18;

# `contains`, `substr`, ... are declared on `Str` and `Cool` only (#13035).
sub err-name(&code) { try code(); $!.^name }

for (a1 => 2), \(1, 2), set(<a>), bag(<a>), v1.2.3, sub foo {} -> $r {
    is err-name({ $r.contains("2") }), 'X::Method::NotFound', "{$r.^name}.contains";
    is err-name({ $r.substr(1) }), 'X::Method::NotFound', "{$r.^name}.substr";
}

# `fmt` and `split` are declared on `Any`, so a Pair still answers them.
is (a => 1).fmt("%s-%s"), 'a-1', 'Pair.fmt';

# A user `Cool` subclass is read through its own `.Str`.
my class C is Cool { method Str { "c1o2,3" } }
my $c = C.new;
is $c.substr(1), '1o2,3', 'substr (1 arg)';
is $c.substr(1, 2), '1o', 'substr (2 args)';
is $c.index("o"), 2, 'index';
is $c.contains("o2"), True, 'contains';
is $c.subst("o", "X"), 'c1X2,3', 'subst';
