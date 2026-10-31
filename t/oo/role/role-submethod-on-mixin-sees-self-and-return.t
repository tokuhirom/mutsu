use Test;

# Found via LN (IO::CatHandle::AutoLines): a role's TWEAK that runs on a `but` /
# parameterized `does` mixin saw `self` as Nil, and a `return` in it escaped
# with "Attempt to return outside of immediately-enclosing Routine", aborting
# the composition.
plan 10;

role Plain { submethod TWEAK { $*seen = self.^name } }
role Param[::T = Any, :$flag = True] { submethod TWEAK { $*seen = self.^name } }
role Early { submethod TWEAK { return unless False; $*seen = "unreached" } }
class K { }

my $*seen = "";
my $a = K.new but Plain;
is $*seen, $a.^name, "but: self in TWEAK is the mixin";

$*seen = "";
my $b = K.new; $b does Param[Int];
ok $*seen.starts-with('K+{Param'), "does Param[Int]: self in TWEAK is the mixin";

$*seen = "";
my $c = K.new; $c does Param[:flag(False)];
ok $*seen.starts-with('K+{Param'), "does Param with a named argument";

$*seen = "";
my $s = "str"; $s does Param[Int];
is $*seen, $s.^name, "plain value";

$*seen = "";
my $d = K.new but Early;
is $*seen, "", "return in TWEAK ends it";
is $d.^name, 'K+{Early}', "and the composition completes (but)";

my $e = K.new; $e does Early;
is $e.^name, 'K+{Early}', "and the composition completes (does)";

# A handle with a role mixed in is still the open handle (LN: `$*ARGFILES does R`).
role Marker { method marker { "m" } }
my $path = $*TMPDIR.add("mixin-handle-{$*PID}.txt");
$path.spurt("first\nsecond\n");
my $fh = $path.open;
$fh does Marker;
is $fh.marker, "m", "role mixed into an open handle";
is $fh.get, "first", "and the handle still reads";
$fh.close;
$path.unlink;

# The mixin's own `print` still wins over the handle's native one (Terminal::ANSI).
{
    my @lines;
    my $saved = $*OUT;
    $*OUT = $*OUT but role { method print($l) { @lines.push($l) } };
    print "captured";
    $*OUT = $saved;
    is @lines.join, "captured", "a role mixed into \$*OUT overrides print";
}
