use v6;
use Test;

# EVAL compilation-unit identity, CompUnit::Repository introspection, and
# `$*RAKU` / `Compiler` reflection.
#
# Every assertion here also passes under the reference `raku`, so this file is
# a compatibility pin, not a mutsu-shaped snapshot. Where mutsu legitimately
# cannot match rakudo (compiler name, version, the exact `verbose-config` key
# set -- all tied to the implementation and its build), the test asserts the
# *shape* rather than the value. Path assertions are relative (`.ends-with`,
# `.IO.basename`) so the file survives being moved.

plan 47;

# ---------------------------------------------------------------------------
# EVAL synthesizes a per-call compilation-unit name for $?FILE
# ---------------------------------------------------------------------------

use MONKEY-SEE-NO-EVAL;

my $outer-file = $?FILE;
is $outer-file.IO.basename, 'eval-compunit-introspection.t',
    '$?FILE in the mainline names this file';

my $e1 = EVAL q[$?FILE];
my $e2 = EVAL q[$?FILE];

isnt $e1, $outer-file, 'EVAL does not inherit the outer unit ($?FILE) name';
isnt $e1, $e2, 'two EVALs get two different compilation-unit names';
ok $e1.IO.is-absolute, '$?FILE inside EVAL is an absolute path';
ok $e2.IO.is-absolute, '$?FILE inside a second EVAL is absolute too';
like $e1.IO.basename, /^ 'EVAL_' \d+ $/, 'the synthesized name is EVAL_<N>';
like $e2.IO.basename, /^ 'EVAL_' \d+ $/, 'and so is the next one';

my $n1 = +$e1.IO.basename.substr(5);
my $n2 = +$e2.IO.basename.substr(5);
is $n2, $n1 + 1, 'the EVAL_<N> counter advances by one per synthesized name';

# The synthesized unit lives under $*CWD.
is $e1.IO.parent.absolute, $*CWD.absolute,
    'the synthesized EVAL unit is named relative to $*CWD';

# ---------------------------------------------------------------------------
# EVAL honors an explicit :filename
# ---------------------------------------------------------------------------

is EVAL(q[$?FILE], filename => '/my-eval-code'), '/my-eval-code',
    'an absolute :filename is used verbatim for $?FILE';

my $rel = EVAL q[$?FILE], filename => 'some/relative/name';
ok $rel.IO.is-absolute, 'a relative :filename is absolutified for $?FILE';
ok $rel.ends-with('some/relative/name'), '... keeping the name it was given';

# An explicit :filename does not consume a synthesized-name counter slot.
my $before = +(EVAL q[$?FILE]).IO.basename.substr(5);
EVAL q[1], filename => 'not-counted';
my $after = +(EVAL q[$?FILE]).IO.basename.substr(5);
is $after, $before + 1, ':filename does not consume an EVAL_<N> counter slot';

# ---------------------------------------------------------------------------
# Code.file is the unit name as-is; $?FILE is its absolute form
# ---------------------------------------------------------------------------

my $pair = EVAL q[sub __f() { }; ($?FILE, &__f.file)];
my ($seen-file, $seen-code-file) = @$pair;
ok $seen-file.IO.is-absolute, '$?FILE inside EVAL is absolute';
is $seen-code-file, $seen-file.IO.basename,
    'Code.file inside EVAL is the bare unit name, not the absolute path';

# Nested EVAL gets its own unit name, distinct from its enclosing EVAL.
my $nested = EVAL q[use MONKEY-SEE-NO-EVAL; ($?FILE, EVAL q<$?FILE>)];
my ($outer-eval, $inner-eval) = @$nested;
isnt $outer-eval, $inner-eval, 'a nested EVAL gets its own compilation-unit name';
like $inner-eval.IO.basename, /^ 'EVAL_' \d+ $/, '... which is also an EVAL_<N>';

# ---------------------------------------------------------------------------
# An EVAL'd snippet's parse sees the outer unit's constants
# ---------------------------------------------------------------------------

constant EvalPreseedConst = 42;
my \EvalPreseedTerm = 43;

is EVAL(q[given 42 { when EvalPreseedConst { "matched-constant" } }]), 'matched-constant',
    'an EVAL parse sees an outer `constant` as a declared term';
is EVAL(q[given 43 { when EvalPreseedTerm { "matched-term" } }]), 'matched-term',
    'an EVAL parse sees an outer sigilless `my \\term` as a declared term';

# ---------------------------------------------------------------------------
# OUR:: exposes the current package's own symbols
# ---------------------------------------------------------------------------

my $ourpkg::member = 7;

ok OUR::.keys.grep(* eq 'ourpkg'),
    'a package created by a qualified declaration is an OUR:: member';
ok !OUR::.keys.grep(* eq 'ourpkg::member'),
    '... exposed as the package, not as a flat qualified key';
is OUR::ourpkg.WHO.keys.sort.join(','), '$member',
    'the sub-package\'s own stash carries its symbols';
is OUR::ourpkg.HOW.^name.split('::').tail, 'PackageHOW',
    'an implicitly created package reports PackageHOW, not ClassHOW';

# ---------------------------------------------------------------------------
# CompUnit::Repository stringification and .files
# ---------------------------------------------------------------------------

my $repo = CompUnit::Repository::FileSystem.new(prefix => $*CWD);
is $repo.short-id, 'file', 'a FileSystem repository has short-id "file"';
is $repo.Str, $repo.prefix.absolute,
    'a repository stringifies as its bare prefix';
is $repo.gist, 'file#' ~ $repo.prefix.absolute,
    'a repository gists as <short-id>#<prefix>';

for $*REPO.repo-chain -> $r {
    # Every repo in the chain renders in the `<short-id>#<prefix>` form rather
    # than the generic `TypeName.new` default gist.
    ok $r.gist.contains('#'), "repo-chain entry gists with a '#' separator";
    last;
}

# `.files` over a real (temporary) distribution.
my $dist-dir = $*TMPDIR.add("mutsu-cu-files-{$*PID}-{now.Int}");
$dist-dir.mkdir;
$dist-dir.add('bin').mkdir;
$dist-dir.add('bin').add('demo-script').spurt("say 'hi'\n");
$dist-dir.add('META6.json').spurt(q:to/META/);
{
  "name": "Demo::Dist",
  "version": "1.2.3",
  "auth": "github:demo",
  "api": "1",
  "provides": { },
  "depends": [ ]
}
META

my $dist-repo = CompUnit::Repository::FileSystem.new(prefix => $dist-dir);

is $dist-repo.files('bin/demo-script').head<name>, 'Demo::Dist',
    '.files finds a distribution providing the requested file';
is $dist-repo.files('bin/demo-script', :ver<1.2.3>).head<name>, 'Demo::Dist',
    '.files matches an exact :ver';
is $dist-repo.files('bin/demo-script', :ver<1.0.0+>).head<name>, 'Demo::Dist',
    '.files matches an open-ended :ver range';
is ($dist-repo.files('bin/demo-script', :ver<419.0+>).head<name> // 'Nada'), 'Nada',
    '.files rejects a version outside the requested range';
is ($dist-repo.files('bin/no-such-file').head<name> // 'Nada'), 'Nada',
    '.files returns nothing for a file the distribution does not provide';
is ($dist-repo.files('bin/demo-script', :auth<github:nobody>).head<name> // 'Nada'), 'Nada',
    '.files rejects a non-matching :auth';
isa-ok $dist-repo.files('bin/no-such-file'), Iterable,
    '.files returns an iterable even when nothing matches';

$dist-dir.add('bin').add('demo-script').unlink;
$dist-dir.add('META6.json').unlink;
$dist-dir.add('bin').rmdir;
$dist-dir.rmdir;

# ---------------------------------------------------------------------------
# $*RAKU / Compiler reflection
# ---------------------------------------------------------------------------

is $*RAKU.^name, 'Raku', '$*RAKU is a Raku, not the pre-rename Perl';
is $*RAKU.Str, 'Raku', '$*RAKU stringifies as Raku';
is $*RAKU.gist, "Raku ({$*RAKU.version})", '$*RAKU gists as its name and version';
is $*RAKU.compiler.^name, 'Compiler', '$*RAKU.compiler is a Compiler';

# The compiler's own identity is implementation-specific; assert only shape.
ok $*RAKU.compiler.name.chars > 0, 'the compiler has a non-empty name';
ok $*RAKU.compiler.id.chars > 0, 'the compiler has a non-empty id';
ok $*RAKU.compiler.version.defined, 'the compiler reports a version';

# `.verbose-config` is a map of section name to a map of config key/value.
# The key *set* is build-specific (rakudo describes a MoarVM build, mutsu its
# own), so only the sections both must be able to answer are asserted.
my %config = $*RAKU.compiler.verbose-config;
ok %config.elems > 0, '.verbose-config returns a non-empty map';
for <Raku kernel distro> -> $section {
    ok %config{$section}.elems > 0, ".verbose-config has a non-empty '$section' section";
}
ok %config<Raku><implementation>.chars > 0,
    '.verbose-config names the implementation';

# ---------------------------------------------------------------------------
# `.put` goes through string context, so a custom .Str is honored
# ---------------------------------------------------------------------------

my $proc = run $*EXECUTABLE, '-e',
    'class C { method Str { "custom-str" } }; C.new.put;', :out, :err;
is $proc.out.slurp(:close).trim, 'custom-str',
    '.put renders through .Str, like .print does';
$proc.err.slurp(:close);

# vim: expandtab shiftwidth=4
