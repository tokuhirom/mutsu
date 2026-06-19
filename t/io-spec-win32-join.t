use Test;

# IO::Spec::Win32.join($volume, $dir, $file) inserts a `\` between a *bare*
# volume and a non-empty directory, but joins a drive volume (`C:`) directly.
# Regression: mutsu concatenated volume + dir without a separator, so
# join('foo','bar','ber') gave "foobar\ber" instead of "foo\bar\ber".

plan 10;

is IO::Spec::Win32.join('foo', 'bar', 'ber'),  'foo\bar\ber', 'bare volume gets a separator';
is IO::Spec::Win32.join('foo', 'bar', ''),     'foo\bar',     'bare volume + dir, no file';
is IO::Spec::Win32.join('foo', '', 'ber'),     'foober',      'empty dir: volume + file directly';
is IO::Spec::Win32.join('foo', '/bar', 'ber'), 'foo/bar\ber', 'dir with leading sep: no extra';
is IO::Spec::Win32.join('foo', '\bar', 'ber'), 'foo\bar\ber', 'dir with leading backslash';
is IO::Spec::Win32.join('C:', 'bar', 'ber'),   'C:bar\ber',   'drive volume joins directly';
is IO::Spec::Win32.join('C:', '/', 'foo'),     'C:/foo',      'drive + root + file';
is IO::Spec::Win32.join('C:', '\foo', 'bar'),  'C:\foo\bar',  'drive + backslash dir';
is IO::Spec::Win32.join('', '/foo', 'bar'),    '/foo\bar',    'empty volume';
is IO::Spec::Win32.join('foo', '', ''),        'foo',         'volume only';
