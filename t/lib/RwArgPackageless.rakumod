# No package declaration: these subs live at the file's top level (mutsu#10520).
use RwArgPackagelessUtil;

sub set-seven($v is rw) { $v = 7 }
sub set-second($min, $v is rw, $max) { $v = 7 }

sub rw-scalar() is export { my $x = 300; set-seven($x); $x }
sub rw-middle() is export { my $x = 300; set-second(0, $x, 255); $x }
sub rw-for-topic() is export { my @x = 300, 1; for @x { set-seven($_) }; @x }
sub rw-imported() is export { my $x = 300; clip-to 0, $x, 255; $x }

class RwArgColor is export {
    has $.g;
    method new(Array() :$rgb) {
        clip-to 0, $_, 255 for @$rgb;
        self.bless: g => $rgb[1]
    }
}
