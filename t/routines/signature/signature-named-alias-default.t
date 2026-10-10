use Test;

# From Distribution::Extension::Updater: `method new(Str $d = '', :d(:$dir) = $d || '.')`.
# A named alias names a caller key only, so the alias name `d` must not shadow
# the sibling positional `$d` while the default is evaluated.

plan 4;

sub a(Str $d = '', :d(:$dir) = $d) { $dir }
is a('abc'), 'abc', 'default reads positional named like the alias';
is a('abc', :d<zz>), 'zz', 'alias key still binds';
is a('abc', :dir<yy>), 'yy', 'leaf name still binds';

sub b(Str $d = '', :d(:$dir) = $d || '.') { $dir }
is b('abc'), 'abc', 'expression default sees the positional';
