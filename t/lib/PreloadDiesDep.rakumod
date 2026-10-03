unit module PreloadDiesDep;

# Registers an exported proto, then dies: the in-place reload after the
# failed BEGIN-time preload used to die on the proto instead (#11351).
my proto sub preload-dies-f(|) is export {*}
my multi sub preload-dies-f(Int) { 1 }
die "PreloadDiesDep body died";
