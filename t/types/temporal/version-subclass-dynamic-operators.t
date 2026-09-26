use Test;

# Version::Raku installs builtin operator routines as methods during BEGIN.
# This keeps both that MOP shape and the Version payload accessors covered.
plan 6;

class VersionLike is Version { }

BEGIN {
    VersionLike.^add_method: 'cmp', &[cmp];
    VersionLike.^add_method: 'eqv', &[eqv];
    VersionLike.^add_method: '==', &[==];
}

my $v = VersionLike.new('1.0');
is-deeply $v.cmp($v), Same, 'a builtin cmp routine can be added as a method';
is-deeply $v.eqv($v), True, 'a builtin eqv routine can be added as a method';
is-deeply $v."=="($v), True, 'a builtin numeric routine can be added as a method';
nok $v.plus, 'a Version subclass delegates plus to its payload';
ok VersionLike.new('1.0').ACCEPTS($v),
    'a Version subclass exposes Version ACCEPTS semantics';
ok VersionLike.new('1.0.*').whatever,
    'a Version subclass delegates whatever to its payload';
