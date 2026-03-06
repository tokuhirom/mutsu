use Test;

plan 4;

my $hash = { :err(''), :out('0,1,2'), :status => 0 };

isa-ok $hash, Hash, "hash literal with colonpairs and fat-arrow entry produces a Hash";
is $hash<err>, "", "colonpair with explicit value is preserved";
is $hash<out>, "0,1,2", "multiple colonpair entries are preserved";
is $hash<status>, 0, "colonpair key followed by => value parses as key/value pair";
