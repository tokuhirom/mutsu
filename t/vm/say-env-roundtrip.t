# Pin test for the Say/Put/Print/Note dual-store round-trip removal (CP-3 slice 1).
# Exercises the actual Say/Put/Print/Note opcodes (via captured stdout/stderr)
# along the paths the old pre-Say `sync_env_from_locals` / post-Say
# `env_dirty = true` round-trip was meant to protect.
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 7;

# 1. say of a freshly-mutated local: the arg comes from the stack, must print 10.
is_run(
    'my $x = 5; $x = 10; say $x;',
    { out => "10\n", status => 0 },
    'say of mutated local prints current value',
);

# 2. custom .gist reading a dynamic var via say
is_run(
    'class G { method gist { "G:" ~ $*GREETING } }; my $*GREETING = "hi"; say G.new;',
    { out => "G:hi\n", status => 0 },
    'say with custom .gist reading dynamic var',
);

# 3. .gist side-effect on a dynamic var; caller observes it after the say
is_run(
    'class S { method gist { $*SEEN = "yes"; "se" } }; my $*SEEN = "no"; say S.new; say $*SEEN;',
    { out => "se\nyes\n", status => 0 },
    'caller observes dynamic-var write done inside .gist during say',
);

# 4. put of a list local
is_run(
    'my @a = 1, 2, 3; put @a;',
    { out => "1 2 3\n", status => 0 },
    'put of array local',
);

# 5. print (no newline) of a mutated local
is_run(
    'my $n = 1; $n = $n + 41; print $n;',
    { out => "42", status => 0 },
    'print of mutated local',
);

# 6. note goes to stderr
is_run(
    'note "warned";',
    { err => "warned\n", status => 0 },
    'note writes to stderr',
);

# 7. say inside a loop reading a per-iteration local
is_run(
    'for 1..3 -> $i { my $sq = $i * $i; say $sq; }',
    { out => "1\n4\n9\n", status => 0 },
    'say of loop-local in each iteration',
);
