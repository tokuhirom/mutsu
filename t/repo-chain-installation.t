use v6;
use Test;

# `$*REPO.repo-chain` must expose the default "site" repository as a
# `CompUnit::Repository::Installation`, the way real Raku's chain always
# contains Installation repositories. zef's `list-installed` / `locate` grep
# the chain for `CompUnit::Repository::Installation` entries; a FileSystem-only
# chain made those commands report nothing installed.

plan 3;

my @chain = $*REPO.repo-chain;
ok @chain.elems >= 1, "repo-chain is non-empty";

my @installs = @chain.grep(CompUnit::Repository::Installation);
ok @installs.elems >= 1,
    "repo-chain contains at least one CompUnit::Repository::Installation";

ok @installs[0].?path-spec.?starts-with("inst#"),
    "the Installation repo reports an inst-prefixed path-spec";
