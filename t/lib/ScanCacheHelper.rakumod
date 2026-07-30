unit module ScanCacheHelper;

enum ScanCacheColor is export <ScanRed ScanBlue>;

class ScanCacheThing is export {
    has $.label = "thing";
}

sub scan-cache-greet(Str $who) is export {
    "hello, $who"
}

sub infix:<scan-cat>($a, $b) is export {
    "$a|$b"
}
