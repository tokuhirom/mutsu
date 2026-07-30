use v6;
use Test;
use Test::Util;

# A `when`/`default` block's final statement is the block's *value* (`when`
# succeeds out of the enclosing topicalizer with it), so it is not in sink
# context. Rakudo warns for `if True { 1 }` but not for `when 'a' { 1 }`.
# mutsu warned for both — and, being a compile-time analysis, it warned even for
# a branch that never runs, so a Linux-only test file emitted a spurious
# "Useless use of constant string ... in sink context" for the `when 'darwin'`
# arm it skipped.

plan 4;

my $when-value = q:to/CODE/;
    given "a" {
        when "a" { 1 }
        when "b" { 2, "hello" }
    }
    say "done";
    CODE

my $default-value = q:to/CODE/;
    given "z" {
        when "a" { 1 }
        default  { "unused" }
    }
    say "done";
    CODE

# Only the LAST statement is the value; earlier ones are still sunk.
my $when-prefix = q:to/CODE/;
    given "a" {
        when "a" { "wasted"; 1 }
    }
    say "done";
    CODE

# An `if` block is different: Rakudo does warn there, so we must keep doing so.
my $if-value = q:to/CODE/;
    if True { 1 }
    say "done";
    CODE

is_run $when-value, { out => "done\n", err => '' }, 'a when block value is not sunk';
is_run $default-value, { out => "done\n", err => '' }, 'a default block value is not sunk';
is_run $when-prefix, { out => "done\n", err => /'sink context'/ }, 'a non-final statement in a when block is still sunk';
is_run $if-value, { out => "done\n", err => /'sink context'/ }, 'an if block value is still sunk';

# vim: expandtab shiftwidth=4
