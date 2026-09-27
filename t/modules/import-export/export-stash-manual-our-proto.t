use Test;

# The "manual EXPORT stash" idiom (see export-stash-manual-our-sub.t / #7988)
# extended to a multi family. Raku rejects `our multi sub` outright ("Cannot
# use 'our' with individual multi candidates. Please declare an our-scoped
# proto instead"), so an `our`-scoped proto is the only legal way to put a
# multi family into an export stash this way:
#
#   unit module Mod;
#   my package EXPORT::DEFAULT {
#       our proto sub delta($) {*}
#       multi sub delta(Int $x) { "delta-int $x" }
#       multi sub delta(Str $x) { "delta-str $x" }
#   }
#
# Before this fix, `delta` parsed (or not, if named as an operator) but
# calling it after `use` always raised "Unknown function": the proto's own
# export bookkeeping ran, but nothing aliased its bare `multi sub` candidates
# (declared afterwards, not themselves `our`-scoped) from the literal
# `EXPORT::DEFAULT::delta/...` registry keys the candidates actually
# registered under to the `{module}::delta/...` keys `import_module` reads.
# See #9720.
#
# Every row measured against raku (v2026.07); this file passes verbatim there
# too.

plan 5;

use lib 't/lib';
use ManualExportStashProtoMod;

is eps(), 'eps', 'a plain our sub inside EXPORT::DEFAULT is still imported';
is delta(3), 'delta-int 3',
    "an our proto sub's multi family in EXPORT::DEFAULT is imported and dispatches (Int)";
is delta("hi"), 'delta-str hi',
    "an our proto sub's multi family in EXPORT::DEFAULT is imported and dispatches (Str)";

use ManualExportStashProtoAllMod :ALL;

is gamma(3), 'gamma-int 3',
    "an our proto sub's multi family in EXPORT::ALL is imported via :ALL (Int)";
is gamma("hi"), 'gamma-str hi',
    "an our proto sub's multi family in EXPORT::ALL is imported via :ALL (Str)";
