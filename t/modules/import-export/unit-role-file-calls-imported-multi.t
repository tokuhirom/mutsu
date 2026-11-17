use lib 't/lib';
use Test;
use ProtoFamilyReexporter;
use ProtoFamilyCallerRole;

# A module with no CHECK phaser has no CHECK phase that could rewrite it, so
# it gets no end-of-compile-phase marker. One after its `use` lines made the
# routine of this `unit role` file find the imported `proto` with no
# candidates (Red t/021: `cache` calling `database`).

plan 2;

is call-open-db('SQLite'), 'db SQLite', 'the role file routine reaches the imported multi';
is open-db('Pg'), 'db Pg', 'the importer reaches it too';
