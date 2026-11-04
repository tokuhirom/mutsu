use Test;

plan 20;

# IO::Spec::Win32.split of a root: the dirname is the separator as written (a
# UNC volume's root is a backslash), the basename is a backslash; a dirname
# loses its trailing separators; a bare drive is the current directory on it.
sub parts($p) { my $s = IO::Spec::Win32.split($p); ($s.volume, $s.dirname, $s.basename) }

is-deeply parts('/'),      ('', '/', '\\'), '/';
is-deeply parts('///'),    ('', '/', '\\'), '///';
is-deeply parts('\\'),     ('', '\\', '\\'), 'a backslash';
is-deeply parts('/\\/'),   ('', '/', '\\'), 'mixed separators keep the first';
is-deeply parts('C:/'),    ('C:', '/', '\\'), 'a drive root';
is-deeply parts('C:\\\\'), ('C:', '\\', '\\'), 'a drive root with backslashes';
is-deeply parts('//server/share/'), ('//server/share', '\\', '\\'), 'a UNC root';
is-deeply parts('//server/share'),  ('//server/share', '\\', '\\'), 'a UNC volume only';
is-deeply parts('//server/share/a/b'), ('//server/share', '/a', 'b'), 'below a UNC volume';
is-deeply parts('//server/share//a'),  ('//server/share', '/', 'a'), 'a root below a UNC volume';
is-deeply parts('/a//b'),  ('', '/a', 'b'), 'trailing separators of the dirname are dropped';
is-deeply parts('a//b'),   ('', 'a', 'b'), 'in a relative path too';
is-deeply parts('/a//'),   ('', '/', 'a'), 'a trailing separator after the name';
is-deeply parts('C:'),     ('C:', '.', ''), 'a bare drive';
is-deeply parts('C:foo'),  ('C:', '.', 'foo'), 'a drive-relative name';
is-deeply parts('C:/a/'),  ('C:', '/', 'a'), 'a name below a drive root';
is-deeply parts(''),       ('', '', ''), 'the empty path';
is-deeply parts('.'),      ('', '.', '.'), 'the dot';
is-deeply parts('./'),     ('', '.', '.'), 'dot slash';
is-deeply parts('foo\\'),  ('', '.', 'foo'), 'a trailing backslash';
