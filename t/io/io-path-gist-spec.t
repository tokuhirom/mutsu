use Test;

# IO::Path.gist shows the absolute path (with the SPEC's separators) when the
# path is absolute, else the path string verbatim. So an absolute Win32 path
# gists with backslashes; Cygwin normalizes to forward slashes.

plan 10;

is IO::Path::Unix.new('foo').gist,       '"foo".IO',       'Unix relative';
is IO::Path::Unix.new('/foo').gist,      '"/foo".IO',      'Unix absolute';
is IO::Path::Win32.new('foo').gist,      '"foo".IO',       'Win32 relative';
is IO::Path::Win32.new('/foo').gist,     '"\foo".IO',      'Win32 absolute uses backslash';
is IO::Path::Win32.new(｢\foo\bar｣).gist, '"\foo\bar".IO',  'Win32 backslash absolute';
is IO::Path::Win32.new('bar/ber').gist,  '"bar/ber".IO',   'Win32 relative keeps slashes';
is IO::Path::Cygwin.new('/foo').gist,    '"/foo".IO',      'Cygwin absolute forward slash';
is IO::Path::QNX.new('/foo').gist,       '"/foo".IO',      'QNX absolute';

# .gist is unaffected by directory changes (uses the stored path, not CWD).
is IO::Path::Unix.new('rel/path').gist,  '"rel/path".IO',  'relative gist verbatim';
is '/abs/path'.IO.gist,                  '"/abs/path".IO', '.IO absolute gist';
