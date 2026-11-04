use Test;

# On Linux rakudo builds $*DISTRO from /etc/os-release: ID is the name,
# HOME_URL the auth, VERSION the version (a Version, via Str.Version),
# VERSION_ID the release and PRETTY_NAME the description; a key the file lacks
# leaves the default. The test reads the same file, so it holds on any
# distribution; elsewhere it only checks the shape.
plan 7;

my %os;
if $*KERNEL.name eq 'linux' && '/etc/os-release'.IO.e {
    for '/etc/os-release'.IO.slurp.subst(:g, '"').lines {
        next unless .contains('=');
        my ($k, $v) = .split('=', 2);
        %os{$k} = $v.trim;
    }
}

if %os {
    is $*DISTRO.name, (%os<ID> // 'linux'), 'name is the os-release ID';
    is $*DISTRO.release, (%os<VERSION_ID> // 'unknown'), 'release is the os-release VERSION_ID';
    is $*DISTRO.desc, (%os<PRETTY_NAME> // 'unknown'), 'desc is the os-release PRETTY_NAME';
    is $*DISTRO.auth, (%os<HOME_URL> // 'unknown'), 'auth is the os-release HOME_URL';
    if %os<VERSION>:exists {
        my $want = %os<VERSION>.Version;
        is $*DISTRO.version.Str, $want.Str, 'version is VERSION converted with .Version';
        is $*DISTRO.version.parts.raku, $want.parts.raku, 'the version keeps its words as parts';
        is $*DISTRO.gist, "{$*DISTRO.name} ($want)", 'gist is name (version)';
    }
    else {
        skip 'no VERSION in os-release', 3;
    }
}
else {
    isa-ok $*DISTRO.version, Version, 'version is a Version';
    ok $*DISTRO.name.chars > 0, 'name is not empty';
    skip 'os-release only', 5;
}
