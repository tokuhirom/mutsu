use Test;

plan 4;

# chmod must accept an IntStr allomorph (e.g. from `<0o777>`), coercing it to
# the inner integer just like a plain Int argument. Regression: the allomorph
# (a Mixin value) fell through to "Invalid mode: 0o777".

my $dir = $*TMPDIR.child("mutsu-chmod-allomorph-{$*PID}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir }; try { $dir.rmdir } }

my $f = $dir.child("f");
$f.spurt: "x";

lives-ok { $f.chmod: 0o644 },      'chmod with octal Int literal';
lives-ok { $f.chmod: <0o755> },    'chmod with IntStr allomorph <0o755>';
lives-ok { $f.chmod: <0o644> },    'chmod with IntStr allomorph <0o644>';

# Int-typed named param binding an allomorph then passed to chmod (the
# Test::Util make-temp-file :chmod path).
sub with-mode(IO::Path $p, Int :$chmod) { $p.chmod: $_ with $chmod }
lives-ok { with-mode($f, :chmod<0o600>) }, 'chmod via Int :$chmod allomorph';
