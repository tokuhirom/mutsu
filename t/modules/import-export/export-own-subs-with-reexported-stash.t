use Test;
use lib 't/lib';
use ExportStashReexportPlusOwn;

# Source: Test::Coverage (t/01-basic.rakutest) checks `MY::<&name>` for each
# export; a module that re-exports another module's stash lost its own
# `is export` subs from the importer's MY::.
plan 5;

ok MY::<&own-plain>, 'own export without a trait is visible in MY::';
ok MY::<&own-asserting>, 'own export with is test-assertion is visible in MY::';
ok MY::<&todo>, 'a re-exported Test routine is visible in MY::';
is own-plain(), 'plain', 'own export is callable';
is own-asserting(), 'asserting', 'trait export is callable';
