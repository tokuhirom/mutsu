use Test;

plan 6;

# Rebinding the topic's source inside a `given`/`with` block that has a
# pointy parameter keeps the rebind: the parameter was never written, so
# nothing is written back over it at block exit.
my class K {
    has Str $!line;
    method set($x) { $!line := $x }
    method with-pointy() {
        with $!line -> $line { $!line := Str; $line } else { 'none' }
    }
    method given-pointy() { given $!line -> $l { $!line := 'z' }; $!line }
}

my $k = K.new;
$k.set('x');
is $k.with-pointy, 'x', 'with -> $line yields the topic';
is $k.with-pointy, 'none', 'the attribute rebound inside the block stays rebound';
$k.set('x');
is $k.given-pointy, 'z', 'given -> $l keeps a := rebind of the attribute';

my $w = 5;
given $w -> $v { $w := 6 }
is $w, 6, 'a lexical source rebound inside the block stays rebound';

# A pointy param that IS written still writes back.
my $x = 5;
given $x -> $v is rw { $v += 10 }
is $x, 15, 'an is rw pointy param written in the block writes back';
my @a = 1, 2;
given @a -> @p { @p.push(3) }
is-deeply @a, [1, 2, 3], 'an array pointy param mutated in the block writes back';
