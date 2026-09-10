use Test;

# `Target($value)` falls back to Raku's `$value.Target` coercion method when the
# target type declares no COERCE/new. mutsu probed for that method with a plain
# "did the call succeed?" test, so an error *raised inside* the method was
# swallowed and reported as "Impossible coercion from 'Any' into 'Target'" --
# a message that names neither the real failure nor even the real source type.
#
# Found via Cro: a chunked response body supply died with "No such method
# 'data'" deep inside `Promise($supply)`, and all the user saw was the bogus
# coercion error.

plan 4;

class Boom {
    method Promise() { die "boom from the coercion method" }
}

throws-like { Promise(Boom.new) }, X::AdHoc,
    message => /'boom from the coercion method'/,
    'an error raised by the coercion method propagates unchanged';

# A *different* method missing inside the coercion method is still that
# method's failure, not "this value cannot coerce".
class InnerMissing {
    method Promise() { 42.no-such-method-here }
}

throws-like { Promise(InnerMissing.new) }, X::Method::NotFound,
    'a method-not-found raised inside the coercion method propagates too';

# A value that genuinely has no coercion method still reports the coercion
# failure, naming its own type.
class Plain { }

throws-like { Promise(Plain.new) }, X::Coerce::Impossible,
    'a value with no coercion method still reports X::Coerce::Impossible';

my $msg = '';
try { Promise(Plain.new) };
$msg = $! ?? $!.message !! '';
ok $msg.contains('Plain'), 'the coercion failure names the source type';
