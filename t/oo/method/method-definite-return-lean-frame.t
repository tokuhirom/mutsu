use Test;

# A planned method with a literal definite return (`--> Nil`, `--> True`,
# `--> False`, `--> Empty`) now runs in the lean frame, which finalizes the
# value exactly as the fast binder does (#18267). Each method is called
# several times so the later calls replay the plan.
plan 8;

class Counter {
    has $.n = 0;
    method add(Int $x --> Nil) { $!n += $x }
    method yes($x --> True) { $!n }
    method no($x --> False) { $!n }
    method none(--> Empty) { $!n }
    method chained(Str:D $s, Bool:D $v --> Nil) { self.add($s.chars + ($v ?? 1 !! 0)) }
}
my $c = Counter.new;
is (^3).map({ $c.add($_).raku }).join(','), 'Nil,Nil,Nil', '--> Nil returns Nil on every call';
is $c.n, 3, '...and the body ran';
is (^2).map({ $c.yes($_) }).join(','), 'True,True', '--> True';
is (^2).map({ $c.no($_) }).join(','), 'False,False', '--> False';
is (^2).map({ $c.none.raku }).join(','), 'Empty,Empty', '--> Empty';
is (^3).map({ $c.chained('ab', $_ %% 2).raku }).join(','), 'Nil,Nil,Nil', 'a --> Nil method that calls another';
is $c.n, 3 + 3 + 2 + 3, '...whose callee wrote the attribute';

class Bare { method stop($x --> Nil) { return } }
is (^2).map({ Bare.new.stop($_).raku }).join(','), 'Nil,Nil', 'a bare return in a --> Nil method';
