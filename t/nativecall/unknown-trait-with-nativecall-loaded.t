use Test;

# NativeCall's `sub EXPORT` hands its importers `&trait_mod:<is>` as the
# dispatcher of one `multi` it declares itself. A trait no candidate accepts
# used to send a call through that dispatcher back into the same by-name
# resolution, forever: the process overflowed its stack instead of reporting
# the unknown trait. It is `Can't use unknown trait` now.
#
# Every expectation was verified against Rakudo.

plan 5;

use NativeCall;

throws-like { EVAL 'sub unknown-one is no-such-trait { 1 }; 1' },
    Exception, message => /'unknown trait'/,
    'an unknown routine trait is an error, not a crash';

multi trait_mod:<is>(Routine $r, :$noted!) { $r.wrap(-> |c { "noted(" ~ callsame() ~ ")" }) }

sub known is noted { 7 }
is known(), 'noted(7)', 'a user trait still applies beside the NativeCall ones';

sub native-pid(--> int32) is native('c', v6) is symbol('getpid') { * }
ok native-pid() > 0, 'and so does is native';

my $d = Q[multi trait_mod:<is>(Routine $r, :$tagged!) { $r.wrap(-> |c { "T<" ~ callsame() ~ ">" }) };];
is EVAL($d ~ Q[sub a is tagged { 1 }; a()]), 'T<1>', 'a trait declared in EVAL applies';
is EVAL($d ~ Q[sub b is tagged { 2 }; b()]), 'T<2>', 'a second EVAL declaring it again applies too';
