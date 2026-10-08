use Test;

plan 8;

# #11902: a `has Str:D @.values` attribute handed over through `bless` must
# keep its identity when read via the accessor, so mutating the accessor
# result mutates the stored array (and keeps the `Str:D` element type).
class F {
    has Str:D @.values;
    method new(:@values) {
        self.bless: values => Array[Str:D].new: |@values
    }
}

my $f = F.new(values => ["a"]);
ok $f.values === $f.values, 'accessor returns the stored array each time';
$f.values.append: "b";
is $f.values.raku, 'Array[Str:D].new("a", "b")', 'append through the accessor';
$f.values.push: "c";
is $f.values.elems, 3, 'push through the accessor';

my @fields = F.new(values => ["a"]);
@fields.first(*.values.elems).values.append: "x".list;
is @fields[0].values.raku, 'Array[Str:D].new("a", "x")', 'append via first(...).values';

class P { has Str:D @.v; }
my $p = P.new(v => Array[Str:D].new("a"));
$p.v.push: "b";
is $p.v.raku, 'Array[Str:D].new("a", "b")', 'default new path';

class U {
    has Int @.v;
    method new { self.bless: v => Array[Int].new(1) }
}
my $u = U.new;
$u.v.push: 2;
is $u.v.raku, 'Array[Int].new(1, 2)', 'undecorated typed attribute';
is $u.v.of.raku, 'Int', 'element type kept';
is $f.values.of.raku, 'Str:D', 'smiley kept in the element type';
