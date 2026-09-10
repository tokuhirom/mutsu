# Pin for todo/deep/is-run-after-raku-read-swallows-child-spawn.md (closed
# 2026-08-20): a t/-resident file's SECOND and later is_run calls whose code
# string reads $*RAKU under an explicit non-default `use v6.x` once failed to
# spawn the child process at all (silent `not ok`, zero diag). The exact
# trigger combination — a t/-resident caller, 2+ is_run calls, at least one
# later call referencing $*RAKU with `use v6.x` — occurred nowhere in the t/
# suite, so a re-emergence would not have been caught by CI. This file IS that
# combination, kept green as the guard.
use Test;
use lib $?FILE.IO.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 6;

is_run 'print "one"', { :out<one> }, 'unrelated is_run first';
is_run 'use v6.c; print $*RAKU.version', { :out<6.c> }, '$*RAKU.version under use v6.c';
is_run 'use v6.e; print $*RAKU.version', { :out<6.e> },
    'second $*RAKU/use v6.x call still spawns its child';
is_run 'print "two"', { :out<two> }, 'unrelated is_run between $*RAKU calls';
is_run 'use v6.e; print $*RAKU.version', { :out<6.e> },
    'repeated use v6.e call still spawns its child';
is_run 'print $*RAKU.version', { :out<6.d> },
    'default language version after explicit use v6.x calls';
