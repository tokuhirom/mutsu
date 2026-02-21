use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 12;

# Basic stdout check
is_run('say 42', { out => "42\n" }, 'basic stdout');

# stderr check
is_run('note "hello"', { err => "hello\n" }, 'basic stderr');

# Multi-line stdout
is_run('say 1; say 2', { out => "1\n2\n" }, 'multi-line stdout');

# Exit status
is_run('exit 1', { status => 1 }, 'exit 1 gives status 1');
is_run('exit 42', { status => 42 }, 'exit 42 gives status 42');
is_run('say "ok"', { status => 0 }, 'normal exit gives status 0');

# Combined checks
is_run('say 42', { out => "42\n", status => 0 }, 'stdout + status');
is_run('say "hello"; note "world"', { out => "hello\n", err => "world\n" }, 'stdout + stderr');

# Regex match on output
is_run('say "hello world"', { out => /hello/ }, 'regex match on stdout');

# die gives non-zero status
is_run('die "boom"', { status => 1 }, 'die gives status 1');

# No test name variant
is_run('say "test"', { out => "test\n" });

# Default status (when not specified and no err, defaults to 0)
is_run('say "abc"; exit 2', { out => "abc\n", status => 2 }, 'exit 2 with output');
