use Test;

# From the ValueTypeCache distribution: `Mu.new` is `self.bless(|%attrinit)`, so
# a `bless` declared in the class or in a composed role runs for `.new`, and
# `self.Mu::bless(|%_)` on an existing instance builds the receiver's class.
plan 8;

my @seen;
class Own {
    has $.x;
    method bless(*%_) { @seen.push('Own'); self.Mu::bless(|%_) }
}
is Own.new(x => 4).x, 4, 'new returns the object bless built';
is-deeply @seen, ['Own'], 'a class-level bless override runs for new';

role Cache {
    has $!key;
    my %cache;
    method !SET-KEY($k) { $!key = $k; self }
    method key { $!key }
    method bless(*%_) {
        my $k = %_<x> // 0;
        %cache{$k} //= self.Mu::bless(|%_)!SET-KEY($k)
    }
}
class Point does Cache { has $.x }

is Point.new(x => 3).key, 3, 'a role-composed bless override runs for new';
ok Point.new(x => 3) === Point.new(x => 3), 'the role cache hands back one object';
isa-ok Point.new(x => 5), Point, 'self.Mu::bless builds the receiver class';
is Point.new(x => 5).^name, 'Point', 'and not a bare Mu';

class Plain { has $.y; method mk(*%_) { self.Mu::bless(|%_) } }
isa-ok Plain.new.mk(y => 1), Plain, 'Mu::bless on an instance builds its class';
is Plain.new.mk(y => 2).y, 2, 'and keeps the named arguments';
