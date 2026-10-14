use Test;

plan 6;

# A rule holding `@( … )` / `$( … )` is a frame like any other, so a code block
# in it runs once per end entered, not once per end computed (#13461).
my @log;

grammar P { regex TOP { <r> 'x' }; regex r { @(<a aa>) { @log.push: $/.to } } }
@log = ();
ok P.parse("aax"), '@( … ) in a plain rule matches';
is @log.join(','), '2', '... entering one end';

grammar Q { regex TOP { <r> 'x' }; regex r { $(rx/ a+ /) { @log.push: $/.to } } }
@log = ();
ok Q.parse("aax"), '$( … ) in a plain rule matches';
is @log.join(','), '2', '... entering one end';

grammar M { regex TOP { <r> 'x' }; regex r { :m @(<a aa>) { @log.push: $/.to } } }
@log = ();
ok M.parse("a\x[301]ax"), '@( … ) in a :m rule over a marked subject matches';
is @log.join(','), '2', '... entering one end';
