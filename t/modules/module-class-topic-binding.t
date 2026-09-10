use v6;
use lib $?FILE.IO.parent.child('lib').Str;
use Test;

# A module whose body registers a class (declared inline or via a nested
# `use`) used to lose `$_` topic binding in map/for/grep blocks inside its
# subs when they were called after the module finished loading: the module's
# package-block exit recorded every new bare env key — including the
# load-time topic — into `package_lexicals`, which GetGlobal consults BEFORE
# env, permanently shadowing each per-iteration topic bind. (Test::META's
# `get-meta` read the leftover `use URI` topic instead of its candidates.)

use TopicClassMod;

plan 4;

is TopicClassMod::map-topic().join(","), "m:a,m:b", 'map topic binds per element';
is TopicClassMod::for-topic().join(","), "f:a,f:b", 'for topic binds per element';
is TopicClassMod::grep-topic(), 2, 'grep topic binds per element';
# Second call still works (the first call must not poison the store either).
is TopicClassMod::map-topic().join(","), "m:a,m:b", 'map topic still binds on the second call';

done-testing;
