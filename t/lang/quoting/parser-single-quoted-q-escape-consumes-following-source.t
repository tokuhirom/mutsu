use Test;

plan 1;

sub consume-event(Str:D $json) { die 'invalid event' }

# Match Rakudo's quote-adverb scan: `\q"` opens a quote whose closing `"` is
# found in the following source, so the first dies-ok call is absorbed into the
# next one. BDD::Behave's EventStream test relies on this parse shape.
dies-ok { consume-event('{"a":"\q"}') }, 'unknown escape';
dies-ok { consume-event('{"a":"open') }, 'unterminated string';

done-testing;
