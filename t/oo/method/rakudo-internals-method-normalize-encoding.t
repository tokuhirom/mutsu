use Test;

plan 14;

# Rakudo::Internals.NORMALIZE_ENCODING: the canonical spelling of an encoding
# name (IO::Socket::Async::SSL's `:enc` calls it).
my %canonical =
    'UTF-8' => 'utf8', 'utf8' => 'utf8', 'Utf-8' => 'utf8',
    'utf-16' => 'utf16', 'UTF16' => 'utf16',
    'utf-16le' => 'utf16le', 'UTF-16-BE' => 'utf16be', 'utf16-le' => 'utf16le',
    'latin-1' => 'iso-8859-1', 'LATIN1' => 'iso-8859-1',
    'utf-8-c8' => 'utf8-c8',
    'windows1252' => 'windows-1252';
for %canonical.sort -> (:$key, :$value) {
    is Rakudo::Internals.NORMALIZE_ENCODING($key), $value, "$key -> $value";
}

is Rakudo::Internals.NORMALIZE_ENCODING('Foo-Bar'), 'foo-bar',
    'an unknown name comes back lowercased';
is Rakudo::Internals.NORMALIZE_ENCODING(Str), 'utf8', 'an undefined argument is utf8';
