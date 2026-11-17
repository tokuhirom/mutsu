use Test;

# A class with a `submethod BUILD` and attribute initializers is constructed
# by the native default constructor: it seeds the initialized attributes,
# runs BUILD, then applies only the initializers BUILD left alone -- the same
# order the full constructor follows. A seed default (the type object the
# parser gives `has Str $.s`) is evaluated once per class and memoized, so
# every construction below is repeated: the first evaluates, the rest reuse.

plan 13;

my $calls = 0;
class Mixed {
    has Str $.s;
    has Int $.i;
    has Bool $.flag = False;
    has $.lock = do { $calls++; Lock.new };
    has $.copy = $!s;
    has $.n = 4;
    submethod BUILD(:$s) { $!s = $s if $s.defined }
}

my @built = (^3).map({ Mixed.new(s => "v$_") });
is @built.map(*.s).join(','), 'v0,v1,v2', 'BUILD sets the attribute it owns';
is @built.map(*.copy).join(','), 'v0,v1,v2',
    'an initializer runs after BUILD and reads its value';
is $calls, 3, 'an initializer with a side effect runs once per construction';
ok @built.map(*.lock).unique(:with(&[===])).elems == 3,
    'each construction gets its own initializer value';

my @plain = (^3).map({ Mixed.new });
is @plain.map({ .s.raku }).join(','), 'Str,Str,Str',
    'an untouched typed attribute holds its type object, every time';
is @plain.map({ .i.raku }).join(','), 'Int,Int,Int', 'the same for Int';
is @plain.map({ .flag.raku }).join(','), 'Bool::False,Bool::False,Bool::False',
    'a literal initializer applies after BUILD';
is @plain.map(*.n).join(','), '4,4,4', 'an integer initializer too';

constant Alias = Int;
class Aliased {
    has Alias $.x;
    submethod BUILD { }
}
is (^3).map({ Aliased.new.x.raku }).join(','), 'Int,Int,Int',
    'a constant-aliased type resolves to its target type object';

class Overrides {
    has Str $.s;
    submethod BUILD { $!s = 'built' }
}
is (^3).map({ Overrides.new.s }).join(','), 'built,built,built',
    'BUILD writing a seeded attribute keeps its value';

class Where {
    has Int $.w where * > 0 = 2;
    submethod BUILD { }
}
is (^2).map({ Where.new.w }).join(','), '2,2',
    'a where-constrained initializer is not checked against its pre-BUILD seed';

class Smiley {
    has Int:D $.d = 3;
    submethod BUILD { }
}
is (^2).map({ Smiley.new.d }).join(','), '3,3',
    'a :D attribute is not rejected for its pre-BUILD seed';

class Tweaked {
    has $.t = 7;
    submethod BUILD { }
    submethod TWEAK { $!t++ }
}
is Tweaked.new.t, 8, 'TWEAK sees the initializer applied after BUILD';
