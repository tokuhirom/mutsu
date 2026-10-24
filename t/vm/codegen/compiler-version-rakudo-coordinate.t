use v6;
use Test;

plan 14;

# ADR-0104 pin. `$*RAKU.compiler.version` / `$*PERL.compiler.version` is a
# language-level coordinate expressed in *Rakudo release dates*, not mutsu's own
# crate version. The ecosystem only ever compares that field against Rakudo
# releases, so answering with mutsu's package version (v0.23.0) makes every such
# gate resolve to its worst branch: `RakudoPrereq` blocked `Proc::Q` from
# loading at all (issue #8403), and `:if($*PERL.compiler.version < v2018.12)`
# shims select their pre-2018 code path even though mutsu implements the modern
# semantics.
#
# mutsu's own build identity stays truthful and reachable in `.name`, `.release`
# and `.id` — it must never claim to *be* rakudo.

my $c = $*RAKU.compiler;

isa-ok $c.version, Version, 'the compiler reports a Version';

# The two gate shapes the ecosystem actually uses.
# 1. RakudoPrereq: `$*PERL.compiler.version before $v` must be False for the
#    Rakudo minimums real distributions ask for.
nok $c.version before v2017.06,
  'the compiler version is not `before` the Rakudo minimum Proc::Q asks for';
nok $c.version before v6.c,
  'the compiler version is not `before` a bare language-revision minimum';

# 2. Conditional-shim loads (Net::BGP's `Conversions-Pre201812`) pick the
#    modern branch.
ok $c.version >= v2018.12,
  'the compiler version selects the post-2018.12 branch of a version shim';
nok $c.version < v2018.12,
  'the compiler version does not select a pre-2018.12 legacy shim';

# 3. The coordinate tracks the Rakudo release the ecosystem oracle and the roast
#    baseline are measured against (2026.09, #14851): `static` starts with
#    `die ... if $*RAKU.compiler.version < v2026.09`.
ok $c.version >= v2026.09,
  'the compiler version passes a `< v2026.09` gate (static)';
is $c.version, v2026.09, 'the compiler version is the measured Rakudo release';

# mutsu never impersonates rakudo: this is what makes `RakudoPrereq`'s
# `rakudo-only` option, and every `compiler.name ne 'rakudo'` check, still
# reject mutsu for the right reason.
is $c.name, 'mutsu', 'the compiler still names itself mutsu, not rakudo';
isnt $c.name, 'rakudo', 'the compiler does not claim to be rakudo';

# The build's own identity did not move: `.release` and `.id` agree about it,
# and neither is a Rakudo release date.
ok $c.release.chars > 0, 'the compiler reports its own release';
ok $c.id.contains($c.release),
  'the compiler id carries the same release the `.release` attribute reports';
is $c.id, Compiler.id, '$*RAKU.compiler.id still matches Compiler.id';

# `$*PERL` is the same process-wide object, so it cannot disagree.
is $*PERL.compiler.version, $c.version,
  '$*PERL.compiler.version agrees with $*RAKU.compiler.version';

# The language revision is a separate surface and is unaffected: it is a `v6.*`
# revision, not a release date.
ok $*RAKU.version < v7, '$*RAKU.version is still a v6 language revision';
