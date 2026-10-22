use Test;

# `Distro.new(...)` takes its attributes from the arguments (RakuPM's
# t/known-failure-nojson.t shadows `$*DISTRO` with `Distro.new(:name('win32'))`
# and branches on it). Expected values are the `raku` output.

plan 9;

my $d = Distro.new(:name<win32>);
is $d.name, 'win32', 'name comes from the argument';
is $d.auth, 'unknown', 'auth defaults to "unknown"';
nok $d.release.defined, 'unspecified attributes are unset';
nok $d.is-win, 'win32 is not mswin32';
ok Distro.new(:name<mswin32>).is-win, 'mswin32 is a Windows distro';

my $e = Distro.new(:name<w>, :release<r>, :auth<a>, :version(v1.2), :desc<d>, :path-sep<S>);
is $e.raku, 'Distro.new(release => "r", path-sep => "S", name => "w", auth => "a", version => v1.2, signature => Blob, desc => "d")', 'every attribute is taken';

ok Distro.new.name.chars, 'Distro.new without arguments is still populated';

{
    my $*DISTRO = Distro.new(:name<win32>);
    sub os { $*DISTRO.name }
    is os(), 'win32', 'a dynamic shadow is seen by callees';
}
isnt $*DISTRO.name, 'win32', 'the shadow ends with its scope';
