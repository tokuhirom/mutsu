# A `sub EXPORT` module whose hook declares a sigilless `my \twice` inside an
# EARLIER bare block. That block is a scope of its own that ends before the
# returned `Map` is built, so `twice` is not a value term the hook can export:
# the exported `&twice` is the unit-scope routine.
use v6.d;

sub twice($a) { $a * 2 }

sub EXPORT(|) {
    { my \twice = 1; }
    Map.new('&twice' => &twice);
}
