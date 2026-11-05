use Test;

plan 8;

# A typed method parameter's constraint resolves without a per-call copy; the
# observable behaviour (accept, reject, type-only enum value) is unchanged.
enum Store <Mem Disk>;
class Point {
    has $.x;
    method dist(Point $o) { ($!x - $o.x).abs }
    method kind(Store) { 'enum' }
    method pos(Positional $p) { $p.elems }
}

my $a = Point.new(x => 3);
my $b = Point.new(x => 10);
is $a.dist($b), 7, 'a class-typed parameter binds an instance';
dies-ok { $a.dist(5) }, 'a class-typed parameter rejects an Int';
is $a.kind(Mem), 'enum', 'a type-only enum-value parameter binds';
dies-ok { $a.kind(5) }, 'a type-only enum-value parameter rejects a mismatch';
is $a.pos((1, 2, 3).map({ $_ })), 3, 'a Positional parameter takes a mapped Seq';

# Definedness of a Failure instance and of type objects still holds.
my $f = (try fail "x");
ok !$f.defined, 'a Failure is undefined';
ok !Point.defined, 'a type object is undefined';
ok $a.defined, 'an instance is defined';
