use v6.d;
use Test;

# A `my sub` declared in a method body must stay callable from a method
# literal created in that body, even when the method runs while another
# package (a class body, e.g. an attribute trait) is the current one.
# AttrX::Mooish installs `anon method`s that call such a helper.
plan 5;

class H {
    method make { my sub gs($x) { "gs$x" }; anon method () { gs(self.^name) } }
    method plain { my sub gp($x) { "p$x" }; method () { gp(1) } }
}

my $top-anon  = H.new.make;
my $top-plain = H.new.plain;
is $top-anon.(Int), 'gsInt', 'anon method calls the local sub from GLOBAL';
is $top-plain.(Int), 'p1', 'method literal calls the local sub from GLOBAL';

my ($in-class-anon, $in-class-plain);
class Bar { BEGIN { $in-class-anon = H.new.make; $in-class-plain = H.new.plain } }
is $in-class-anon.(Int), 'gsInt', 'anon method made inside a class body works';
is $in-class-plain.(Int), 'p1', 'method literal made inside a class body works';

multi trait_mod:<is>(Attribute:D $attr, :$helper!) {
    H.new.make.(Int) eq 'gsInt' or die 'local sub lost';
    $*PACKAGE.^add_method('hello', H.new.make);
}
class Baz { has $.x is helper; }
is Baz.new.hello, 'gsBaz', 'method installed from an attribute trait finds its local sub';
