use Test;

plan 10;

class Foo is rw {
    has $.x;
    has $.y;
    has $!z = 1;

    method z() {
        $!z;
    }

    method bump-z() {
        $!z = $!z + 1;
    }
}

my $f = Foo.new(x => 1, y => 2, z => 3);
is $f.x, 1, 'reader works for public attribute';

$f.x = 10;
is $f.x, 10, 'class-level is rw enables write on $.x';

$f.y += 5;
is $f.y, 7, 'class-level is rw enables compound assignment on $.y';

lives-ok { $f.x++ }, 'class-level is rw enables postfix ++ on accessor';
is $f.x, 11, 'postfix ++ updates the accessor-backed value';

dies-ok { $f.z = 9 }, 'private attribute is still not writable via public accessor';

$f.bump-z();
is $f.z, 4, 'private attribute mutates through method as usual';

class Baz {
    has $.a;
    has $.a_ro is readonly;
    also is rw;
}

my $b = Baz.new(a => 12, a_ro => 42);
lives-ok { $b.a++ }, q{'also is rw' enables rw accessors};
is $b.a, 13, q{'also is rw' applies to attributes declared before it};
dies-ok { $b.a_ro++ }, q{'is readonly' still overrides class-level rw};
