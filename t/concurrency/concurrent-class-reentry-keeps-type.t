use Test;

# A class declared in a routine body is one type per declaration: its body's
# statements (`my` variables, nested subs) run on every entry, the type is
# made once (#18070). Rebuilding it per entry raced with other threads
# dispatching on it through the shared type registry (ADR-17216): a caller
# found a method table being refilled.

plan 7;

my @bad;
my $lock = Lock.new;
sub with-anon-class($i) {
    my $c = class :: { my $z = 1; method put(|) { "put" }; method print(|) { 'print' } };
    my $p = $c.put;
    my $q = $c.print;
    $lock.protect({ @bad.push($i) }) unless $p eq 'put' && $q eq 'print';
    $p ~ $q ~ $i;
}
await (^8).map: -> $i { start { (^25).map({ with-anon-class($i) }).tail } };
is @bad.elems, 0, 'no thread saw a half-rebuilt anonymous class';

my @entries;
sub f($i) {
    my class C {
        my $z = $i;
        @entries.push($i);
        sub helper { $z * 10 }
        method z { $z }
        method h { helper() }
    };
    C
}
my $a = f(1);
my $b = f(2);
ok $a === $b, 'two entries give the same type object';
is @entries.join(','), '1,2', 'the body ran on every entry';
is $b.z, 2, 'the methods see the latest entry';
is $b.h, 20, 'a body sub sees the latest entry';

my @named = await (^4).map: -> $i {
    start { (^20).map({ f($i).z }).List }
};
is @named.map(*.elems).sum, 80, 'concurrent entries of a my class with body statements';

sub g() { my class D { has $.v = 5; my $w = 7; method both { $!v + $w } }; D }
is g().new.both, 12, 'attributes and body statics of a kept type';
