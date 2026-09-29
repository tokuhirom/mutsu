use Test;

plan 8;

my $cwd = $*CWD.Str;
is IO::Spec::Unix.rel2abs('/foo/'), '/foo',
    'Unix rel2abs removes a trailing separator from an absolute path';
is IO::Spec::Unix.rel2abs('foo', 'bar'),
    IO::Spec::Unix.canonpath($cwd ~ '/bar/foo'),
    'Unix rel2abs resolves a relative base against the cwd';
is IO::Spec::Cygwin.rel2abs('//t1/t2/t3', '/foo'), '//t1/t2/t3',
    'Cygwin rel2abs preserves a leading double slash';
is IO::Spec::Win32.split('/foo/').raku,
    'IO::Path::Parts.new("","/","foo")',
    'Win32 split preserves the leading slash used by the path';
is IO::Spec::Win32.split('').raku,
    'IO::Path::Parts.new("","","")',
    'Win32 split of an empty path has empty parts';
is-deeply IO::Spec::Win32.splitpath('.'), ('', '', '.'),
    'Win32 splitpath treats a single dot as the file';
is IO::Spec::Win32.join('//server/share', ｢\｣, '/'), '//server/share',
    'Win32 join does not append a separator to a slash-style UNC volume';
is IO::Spec::Win32.rel2abs('foo', 'bar'),
    IO::Spec::Win32.canonpath($cwd ~ '/bar/foo'),
    'Win32 rel2abs resolves a relative base against the cwd';
