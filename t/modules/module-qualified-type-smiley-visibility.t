use Test;
use lib 't/lib';
use SmileyVis::Root;
use SmileyVis::Thing;

# A module routine naming its own qualified class with a smiley
# (`SmileyVis::Thing:D`) resolves it like the bare name, even when another
# compunit provides the top-level `SmileyVis` module (#18254; LLM::Tooling's
# `@tools.all ~~ LLM::Tool:D` died "Could not find symbol").

plan 5;

ok is-defined-thing(SmileyVis::Thing.new), ':D matches an instance';
nok is-defined-thing(SmileyVis::Thing), ':D rejects the type object';
ok is-thing-type(SmileyVis::Thing), ':U matches the type object';
ok all-things([SmileyVis::Thing.new, SmileyVis::Thing.new]), 'a junction against :D';
ok SmileyVis::Thing.new ~~ SmileyVis::Thing:D, 'the importer names it with a smiley too';
