use v6;
use Test;

# From the Web::Scraper distribution: `%.d = Hash.new` (the `.` twigil form of
# a whole-aggregate attribute assignment) inside a method that runs while
# another instance's method is mid-flight used to store into the CALLER's
# attribute container, so two objects ended up sharing one hash.

plan 6;

class Dyn {
    has %.d is rw;
    has @.a is rw;
    has $.other;
    method reset { %.d = Hash.new; @.a = [] }
    method run($main) { %.d = Hash.new; $main() }
    method nest {
        %.d = (a => 1);
        @.a = (1, 2);
        $!other.reset;
        (%.d.raku, @.a.raku, $!other.d.raku).join(' ')
    }
}

my $i = Dyn.new;
my $o = Dyn.new(other => $i);
is $o.nest, '{:a(1)} [1, 2] {}', 'a nested call on another instance leaves the caller\'s attributes alone';

my ($o2, $i2) = Dyn.new, Dyn.new;
$o2.run({ $o2.d<a> = 1; $i2.run({ 1 }); $i2.d<b> = 1 });
is $o2.d.raku, '{:a(1)}', 'caller attribute survives the inner run';
is $i2.d.raku, '{:b(1)}', 'inner attribute holds only its own key';

my ($o3, $i3) = Dyn.new, Dyn.new;
$o3.d<a> = 1;
$o3.run({ $i3.reset; $o3.d<c> = 3 });
is $o3.d.raku, '{:c(3)}', 'run resets only its own hash';
is $i3.d.raku, '{}', 'the other instance is reset independently';

my $d = Dyn.new;
$d.d = (x => 1);
is $d.d.raku, '{:x(1)}', 'plain accessor assignment still works';

done-testing;
