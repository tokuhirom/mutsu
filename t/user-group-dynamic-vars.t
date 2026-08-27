use Test;

# Regression pin for todo/tickets/user-group-dynamic-variables-missing.md:
# $*USER and $*GROUP used to be unimplemented and read as Nil, which made
# `+$*USER` silently return 0 (i.e. "root") instead of erroring -- a
# permission check in user code could take the wrong branch. Both are
# IntStr allomorphs (Rakudo: `getpwuid(geteuid())` / `getgrgid(getegid())`),
# so `say $*USER` prints the login/group name while `+$*USER`/`$*USER == 0`
# reads the numeric uid/gid.
#
# This file must pass under both `raku` and `mutsu`, and must keep passing in
# CI, which runs as a different user than any given dev box -- so it asserts
# structural facts (type, numeric/string agreement, cross-check against the
# `id` command) rather than hardcoding a username or uid.

plan 10;

is $*USER.^name, 'IntStr', '$*USER is an IntStr allomorph';
is $*GROUP.^name, 'IntStr', '$*GROUP is an IntStr allomorph';

ok +$*USER ~~ Int, '+$*USER is an Int';
ok +$*GROUP ~~ Int, '+$*GROUP is an Int';

ok ~$*USER.chars > 0, '~$*USER is a non-empty Str';
ok ~$*GROUP.chars > 0, '~$*GROUP is a non-empty Str';

# The numeric and string facets of the same allomorph agree with each other.
ok $*USER == +$*USER, '$*USER == +$*USER (numeric facet)';
ok $*USER eq ~$*USER, '$*USER eq ~$*USER (string facet)';

# Cross-check the numeric facet against the OS's own idea of the effective
# uid/gid, obtained a completely different way (shelling out to `id`) so this
# never hardcodes this box's own uid/gid.
my $id_u = run('id', '-u', :out).out.slurp(:close).trim;
my $id_g = run('id', '-g', :out).out.slurp(:close).trim;
is +$*USER, $id_u.Int, '+$*USER matches `id -u`';
is +$*GROUP, $id_g.Int, '+$*GROUP matches `id -g`';
