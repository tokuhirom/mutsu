use v6;
use Test;

# From App::Racoco::Configuration (ecosystem): inside a `unit module`, a role
# doing a sibling parametric role whose argument is a `::`-qualified type
# (`does Key[IO::Path]`) reported "Unknown role: Key[IO::Path]".
plan 1;

my $code = q:to/CODE/;
    unit module Foo;
    role Key[::T] { has Str $.name is required; method convert(Str $v --> T) { ... } }
    role PathKey does Key[IO::Path] is export {
        method convert($value --> IO::Path) { $value.IO }
    }
    class FP does PathKey is export { }
    say FP.new(name => "a").convert("x").^name;
    CODE
my $f = $*TMPDIR.add("mutsu-unit-role-{$*PID}.raku");
$f.spurt($code);
my $p = run $*EXECUTABLE, $f, :out, :err;
LEAVE $f.unlink;
is $p.out.slurp.trim, "IO::Path", 'sibling parametric role with qualified type argument';
