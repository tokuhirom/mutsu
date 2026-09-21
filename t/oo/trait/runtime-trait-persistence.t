use Test;

role FirstRuntimeAttributeTrait { }
role SecondRuntimeAttributeTrait { }

class RuntimeTraitHolder {
    has $.value;
}

my $attribute = RuntimeTraitHolder.^attributes.first;
$attribute does FirstRuntimeAttributeTrait;
$attribute does SecondRuntimeAttributeTrait;
my $refreshed = RuntimeTraitHolder.^attributes.first;

plan 3;
ok $refreshed ~~ FirstRuntimeAttributeTrait, 'the first runtime attribute trait persists';
ok $refreshed ~~ SecondRuntimeAttributeTrait, 'a later runtime attribute trait preserves the first';
is $refreshed.name, '$!value', 'the refreshed object remains the same attribute';
