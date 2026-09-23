use Test;

plan 2;

# Ujumla relies on stateful grammar actions for scoped interpolation and
# include processing. Keep different quantified named captures interleaved in
# source order when walking a completed grammar match.
grammar G {
    rule TOP { [ <line> | <section> ]+ }
    token line { \w+ '=' \w+ }
    token section { '<' \w+ '>' }
}

class Actions {
    has @.events;

    method line($/) { @!events.push: "line:{ ~$/.Str }" }
    method section($/) { @!events.push: "section:{ ~$/.Str }" }
}

my $actions = Actions.new;
my $match = G.parse('a=1 <x> b=2 <y>', :$actions);

ok $match.defined, 'interleaved named captures parse';
is $actions.events.join('|'), 'line:a=1|section:<x>|line:b=2|section:<y>',
    'grammar actions run in source order across capture names';
