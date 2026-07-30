use v6;
use Test;

# `$*VM.config` is not a decorative hash: NativeLibs switches its whole
# library-naming scheme on `config<osname>` and builds a C command line out of
# the toolchain keys. A missing key made every platform branch silently fall
# through instead of failing loudly.

plan 11;

my $c = $*VM.config;

isa-ok $c, Hash, 'config is a Hash';

ok $c<osname>.defined, 'config<osname> is defined';
is $c<osname>, $*VM.osname, 'config<osname> agrees with $*VM.osname';
ok $c<osname> ~~ /^ <[a..z0..9]>+ $/, "config<osname> is a bare lowercase name ({$c<osname>})";

ok $c<be> eq '0' | '1', 'config<be> is the string 0 or 1';
ok $c<nativecall_backend>.defined, 'config<nativecall_backend> is defined';

# The C toolchain keys. Every one of them is joined into a shell command by
# NativeLibs::Compile, so an undefined value would stringify to a warning and
# produce a broken command line.
my @toolchain = <cc ccshared cflags ccout obj ld ldshared ldflags ldlibs ldout dll>;
my @missing = @toolchain.grep({ !$c{$_}.defined });
is @missing.join(' '), '', 'every C toolchain key is defined';
ok $c<cc>.chars > 0, 'config<cc> names a compiler';
is $c<ccout>, $c<ldout>, 'ccout and ldout agree (both are the -o switch)';

# `dll` is a sprintf pattern: exactly one %s, and it must produce the same
# name `platform-library-name` does for a bare stem.
ok $c<dll>.contains('%s'), 'config<dll> is a sprintf pattern';
is sprintf($c<dll>, 'foo'), $*VM.platform-library-name('foo'.IO).Str,
   'config<dll> agrees with platform-library-name';

# vim: expandtab shiftwidth=4
