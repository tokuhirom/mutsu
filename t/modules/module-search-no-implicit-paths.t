use v6;
use Test;

plan 6;

# Module resolution searches `use lib` -> `-I` -> MUTSULIB -> installed repos ->
# bundled batteries, and nothing else (#11213). It used to also search, ahead of
# the bundled batteries, the script's own directory and every ancestor's
# `packages/<Top>/lib` (a roast `Test::Util` convenience). Rakudo searches
# neither, and both let a stray file near a script silently replace a module.

my $exe  = $*EXECUTABLE.absolute;
my $root = $*TMPDIR.add("mutsu-11213-{$*PID}-{now.Num.subst('.', '')}");
$root.mkdir;

sub write-module(IO::Path $dir, Str $name, Str $marker) {
    $dir.mkdir;
    $dir.add("$name.rakumod").spurt:
        "unit module $name;\nsub encode-base64(|) is export \{ '$marker' \}\n"
        ~ "sub probe-it() is export \{ '$marker' \}\n";
}

sub run-script(IO::Path $script, Str $code) {
    $script.parent.mkdir;
    $script.spurt($code);
    # Run from an unrelated directory so the current directory plays no part.
    my $r = run($exe, $script.absolute, :out, :err, :cwd($root));
    my $out = $r.out.slurp(:close).trim;
    my $err = $r.err.slurp(:close).trim;
    $r.exitcode == 0 ?? $out !! "[exit {$r.exitcode}] $err"
}

# A module next to the script.
{
    my $sdir = $root.add('sdir');
    write-module($sdir, 'Base64', 'from-script-dir');
    write-module($sdir, 'Probe11213', 'from-script-dir');

    is run-script($sdir.add('bundled.raku'),
            'use Base64; say encode-base64("hi", :str);'),
        'aGk=', "a same-named file next to the script does not shadow a bundled module";
    like run-script($sdir.add('missing.raku'), 'use Probe11213; say probe-it();'),
        /'Could not find Probe11213'/, "the script's own directory is not searched";
    is run-script($sdir.add('with-lib.raku'),
            'use lib $?FILE.IO.parent; use Probe11213; say probe-it();'),
        'from-script-dir', 'an explicit `use lib` still reaches it';
}

# A `packages/<Top>/lib` tree above the script.
{
    my $search = $root.add('search');
    write-module($search.add('packages/Base64/lib'), 'Base64', 'from-ancestor-packages-dir');
    write-module($search.add('packages/Probe11213/lib'), 'Probe11213', 'from-ancestor-packages-dir');
    write-module($search.add('roast/packages/Probe11213-Helpers/lib'), 'Probe11213',
        'from-ancestor-roast-packages-dir');
    my $deep = $search.add('a/b');

    is run-script($deep.add('bundled.raku'),
            'use Base64; say encode-base64("hi", :str);'),
        'aGk=', 'an ancestor packages/ tree does not shadow a bundled module';
    like run-script($deep.add('missing.raku'), 'use Probe11213; say probe-it();'),
        /'Could not find Probe11213'/, 'ancestor packages/ trees are not searched';
    is run-script($deep.add('with-lib.raku'),
            'use lib $?FILE.IO.parent(3).add("packages/Probe11213/lib"); '
            ~ 'use Probe11213; say probe-it();'),
        'from-ancestor-packages-dir', 'an explicit `use lib` on the package still reaches it';
}

sub rm-tree(IO::Path $p) {
    if $p.d { rm-tree($_) for $p.dir; $p.rmdir } else { $p.unlink }
}
rm-tree($root);
