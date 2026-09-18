use v6;
use lib 't/lib';
use Test;

# ecosystem `IP::Addr`: `IP::Addr::v4.rakumod` does `use IP::Addr::Handler;
# use IP::Addr::Common;`, where Handler itself already `use`s Common. By the
# time the direct `use Common` runs, Common is already loaded (transitively,
# via Handler), so it takes the "already loaded" short-circuit path in
# `use_module_with_tags_inner` instead of a fresh load. A nested `grammar
# ... does <RoleFromCommon>` declared afterward then failed to compose with
# X::InvalidType: Invalid typename, even though the role's bare name resolved
# fine as an ordinary term (`Role.^name`) moments earlier -- `does`/`is`/
# `hides` parent resolution (`resolve_declared_type_name`) checked `env` but
# not the `package_type_alias` fallback that general type-name resolution
# (`has_type`) already consults for exactly this "imported under a re-use"
# case.
#
# ReusedRoleHandler mirrors IP::Addr::Handler (its own body `use`s
# ReusedRoleCommon first); ReusedRoleUser mirrors IP::Addr::v4 (`use
# ReusedRoleHandler; use ReusedRoleCommon;` then a nested grammar composing
# the role Common exports). ReusedRoleCommon's own mismatch between its file
# name (how it is `use`d) and its declared `unit module` name mirrors the
# real IP::Addr::Common.rakumod (which is `use`d as `IP::Addr::Common` but
# declares `unit module IP::Addr::Const;`) -- that mismatch turned out to be
# essential to reproducing the bug; a module whose file name and declared
# package agree did not trigger it.
use ReusedRoleUser;

plan 1;

is ReusedRoleUser.new.greet-via-grammar, 'hi-from-role',
    'a nested grammar composes a role re-used from an already-loaded transitive dependency';
