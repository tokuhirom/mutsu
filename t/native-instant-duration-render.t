use Test;

# VM-native gist/Str/raku rendering for Instant and Duration instances
# (Track A native-method dispatch). Previously these fell through to the
# interpreter — Instant because the dispatch arms only covered raku/perl, and
# Duration additionally because the VM's Numeric-bridge bypass diverted every
# method on a Real instance to the interpreter.

plan 14;

# --- Instant ---
my $i = Instant.from-posix(1000000);
is $i.gist, 'Instant:1000010', 'Instant.gist';
is $i.Str, 'Instant:1000010', 'Instant.Str';
is ~$i, 'Instant:1000010', 'Instant in string context';
is $i.raku, 'Instant.from-posix(1000000.0)', 'Instant.raku';

# --- Duration (integer) ---
my $d = DateTime.new(2020, 1, 1, 0, 0, 30) - DateTime.new(2020, 1, 1, 0, 0, 0);
is $d.^name, 'Duration', 'subtracting DateTimes yields a Duration';
is $d.gist, '30', 'Duration.gist (integer)';
is $d.Str, '30', 'Duration.Str (integer)';
is $d.raku, 'Duration.new(30.0)', 'Duration.raku (integer)';

# --- Duration (fractional) ---
my $df = DateTime.new(2020, 1, 1, 0, 0, 30.5) - DateTime.new(2020, 1, 1, 0, 0, 0);
is $df.gist, '30.5', 'Duration.gist (fractional)';
is $df.Str, '30.5', 'Duration.Str (fractional)';

# Duration arithmetic still bridges through Numeric (bypass preserved for
# non-render methods)
ok ($d + 0) == 30, 'Duration still participates in numeric ops';
is $d.Int, 30, 'Duration.Int via the Numeric bridge';

# A user class that does Real (no custom render) renders via the Numeric bridge
# and still participates in arithmetic — the bypass relaxation does not change
# its behavior.
class Plain does Real {
    has $.n;
    method Bridge { $!n.Num }
}
my $p = Plain.new(n => 7);
is $p.gist, '7', 'user Real class default gist bridges to the number';
ok ($p + 5) == 12, 'user Real class still bridges arithmetic';

done-testing;
