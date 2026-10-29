use Test;

# From WriteOnceHash 0.0.9 (`my %owoh{Any} does WriteOnce`): an Int key on a
# hash a role was mixed into is an associative key, and `does` keeps the
# container's parameterization.

plan 7;

role R { }

my %o does R;
%o{42} = "a";
is %o{42}, "a", '%h{42} on a role-mixed hash reads the Int key';
is %o.keys.List, (42,), 'the Int key was stored as a key';
nok %o{43}.defined, 'a missing Int key is undefined';

my %p{Str} does R;
%p<x> = 1;
is %p.^name, 'Hash[Any,Str]+{R}', 'does keeps the object-hash key type';

my %q{Any} does R;
is %q.^name, 'Hash[Any,Any]+{R}', 'does keeps the Any key type';
%q{42} = "b";
is %q{42}, "b", 'object hash with Int key';

my %r of Int does R;
is %r.^name, 'Hash[Int]+{R}', 'does keeps the value type';
