use Test;

# #14588 (ValueClass's HOW): while a HOW's `compose` runs, the class has not
# computed its MRO yet, so `find_method` sees only what the class itself
# declares -- not `Mu`'s `WHICH` -- and the hook can add its own.
use lib 't/lib';
use FindMethodComposeHow;

plan 3;

my composed-how Seen { has $.x }

nok $FindMethodComposeHow::SEEN, 'find_method does not see Mu::WHICH before compose';
ok Seen.^find_method("WHICH", :local).defined, 'after the hook the added WHICH is found';
is Seen.new.WHICH.Str, "added|Seen", 'and it answers';
