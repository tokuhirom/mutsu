use Test;

# `grammar P::Ext { use P; also is P }`: the parent comes into scope through
# the body's own `use`. A fresh compile runs that `use` at parse time; a module
# restored from the precompilation cache runs it only when the body runs, which
# is after the class header registered its parents. When an earlier
# `P::Util` had already declared `P` as a stub package, the warm run rejected
# the parent as an inheritance-less package ("P does not support inheritance")
# instead of deferring the `also is` to the body. From
# CSS::Specification::Extended, loaded by CSS::Specification::Compiler. The
# second run is the one that matters, so this test keeps its own cache.
plan 4;

my $lib = 't/lib';
my $cache = $*TMPDIR.add("mutsu-also-is-stub-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use AlsoIsStub::Zed;
    say AlsoIsStub::Zed.parse-it('42');
    PROBE

%*ENV<XDG_CACHE_HOME> = $cache.Str;

sub run-probe() {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    my $err = $proc.err.slurp;
    $proc.out.slurp.chomp || $err.lines.head;
}

is run-probe(), '42', 'cold run: the body-level `use` brings in the parent grammar';
is run-probe(), '42', 'warm run from the precompilation cache does the same';

# A real package is still no parent, whether named in the header or the body.
package NotAClass { }
throws-like 'class InHeader is NotAClass { }', X::Inheritance::Unsupported,
    message => 'NotAClass does not support inheritance, so InHeader cannot inherit from it';
throws-like 'class InBody { also is NotAClass }', X::Inheritance::Unsupported,
    message => 'NotAClass does not support inheritance, so InBody cannot inherit from it';

END {
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
