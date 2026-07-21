use v6;
use Test;

# `sub EXPORT` custom module export: a module may define `sub EXPORT(...)` which
# is called at load time with the `use` arguments; the Map(s) it returns name
# the symbols installed into the importing scope. This coexists with `is export`.
# (Exported sub names avoid Raku builtins like `val` so a bareword call resolves
# to the export, not the builtin.)

plan 14;

# A scratch lib directory for the generated modules.
my $lib = $*TMPDIR.add("mutsu-sub-export-{$*PID}");
$lib.mkdir;
LEAVE { try { .unlink for $lib.dir; $lib.rmdir } }

sub write-mod($name, $body) {
    $lib.add("$name.rakumod").spurt($body);
}

sub run-out($code) {
    run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out).out.slurp;
}

# 1) No-arg EXPORT returning a sub.
write-mod 'ENoArg', q:to/END/;
    sub EXPORT() { Map.new: '&greet' => sub { "hi" } }
    END
is run-out('use ENoArg; print greet()'), 'hi',
    'no-arg EXPORT installs an exported sub';

# 2) EXPORT receiving a single `use` argument, used in the returned closure.
write-mod 'EArg', q:to/END/;
    sub EXPORT($x) { Map.new: '&expanded' => sub { "got:$x" } }
    END
is run-out('use EArg "hello"; print expanded()'), 'got:hello',
    'EXPORT arg is captured by a returned closure';

# 3) A `<a b c>` word list flattens into positional EXPORT args.
write-mod 'EWords', q:to/END/;
    sub EXPORT(*@a) { Map.new: '&joined' => sub { @a.join(",") } }
    END
is run-out('use EWords <a b c>; print joined()'), 'a,b,c',
    'a word list flattens into positional EXPORT args';

# 4) EXPORT can export scalars, arrays, hashes and subs together.
write-mod 'EMix', q:to/END/;
    sub EXPORT() {
        Map.new: '$S' => 7, '@A' => [1, 2], '%H' => { a => 1 }, '&f' => sub { "f" }
    }
    END
is run-out('use EMix; print "$S|@A[]|%H<a>|{f()}"'), '7|1 2|1|f',
    'EXPORT installs scalar/array/hash/sub symbols';

# 5) A local defined in EXPORT is captured by a returned closure.
write-mod 'ELocal', q:to/END/;
    sub EXPORT() { my $y = "localval"; Map.new: '&peek' => sub { "got:$y" } }
    END
is run-out('use ELocal; print peek()'), 'got:localval',
    'a closure the EXPORT returns captures an EXPORT local';

# 6) `sub EXPORT` and `is export` coexist.
write-mod 'EBoth', q:to/END/;
    sub EXPORT() { Map.new: '&fromexport' => sub { "E" } }
    sub fromtrait is export { "T" }
    END
is run-out('use EBoth; print fromexport() ~ fromtrait()'), 'ET',
    '`sub EXPORT` and `is export` coexist';

# 7) EXPORT itself is not leaked as a callable.
{
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e',
        'use ENoArg; EXPORT()', :out, :err);
    $proc.out.slurp;
    $proc.err.slurp;
    isnt $proc.exitcode, 0, 'EXPORT is not importable as a callable';
}

# 8) EXPORT side effects (note/die) run at use time.
write-mod 'EDie', q:to/END/;
    sub EXPORT($v) { die "prereq failed: $v" if $v eq 'bad'; Map.new }
    END
{
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e',
        'use EDie "bad"; say "unreached"', :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    isnt $proc.exitcode, 0, 'EXPORT that dies aborts the load';
    unlike $out, /unreached/, 'code after a dying use does not run';
    like $err, /'prereq failed: bad'/, 'EXPORT die message is reported';
}

# 9) A no-EXPORT module is unaffected (regression guard).
write-mod 'EPlain', q:to/END/;
    sub plainsub is export { "plain" }
    END
is run-out('use EPlain; print plainsub()'), 'plain',
    'a module without EXPORT still imports normally';

# 10) EXPORT args can be dynamic expressions, not just literals.
write-mod 'EDyn', q:to/END/;
    sub EXPORT($x) { Map.new: '&echo' => sub { $x } }
    END
is run-out('my $v = "runtime-" ~ (3 + 4); use EDyn $v; print echo()'),
    'runtime-7', 'EXPORT receives a runtime-evaluated argument';

# 11) EXPORT selecting among existing module subs by argument.
write-mod 'ESelect', q:to/END/;
    sub greet-en { "hello" }
    sub greet-fr { "bonjour" }
    sub EXPORT($lang) {
        Map.new: '&hail' => ($lang eq 'fr' ?? &greet-fr !! &greet-en)
    }
    END
is run-out('use ESelect "fr"; print hail()'), 'bonjour',
    'EXPORT selects among existing module subs by argument';

# 12) EXPORT locals do not leak into the caller's lexical scope.
write-mod 'ENoLeak', q:to/END/;
    sub EXPORT() { my $secret = "hidden"; Map.new: '&ok' => sub { "ok" } }
    END
is run-out('use ENoLeak; print ok(); print "|"; print ($secret // "clean")'),
    'ok|clean', 'EXPORT locals do not leak into the caller scope';
