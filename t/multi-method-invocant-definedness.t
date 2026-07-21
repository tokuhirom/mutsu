use v6;
use Test;

plan 10;

# multi-dispatch on the invocant's definedness via anonymous :D:/:U: markers
class Cook {
    has @.utensils is rw;
    multi method gist(Cook:U:) { 'U:' ~ self.^name }
    multi method gist(Cook:D:) { 'D:' ~ @.utensils.join(',') }
}
is Cook.gist,                        'U:Cook',  'Cook:U: candidate on a type object';
is Cook.new(utensils => <a b>).gist, 'D:a,b',   'Cook:D: candidate on an instance';

# The same, with a named invocant carrying the smiley
class C {
    multi method g(C:U $s:) { 'u' }
    multi method g(C:D $s:) { 'd' }
}
is C.g,     'u', 'named :U invocant on type object';
is C.new.g, 'd', 'named :D invocant on instance';

# A single (non-multi) :D: method still binds self and enforces the constraint
class D {
    method who(D:D:) { self.^name }
}
is D.new.who, 'D', 'single :D: method binds self';
dies-ok { D.who }, ':D: method rejects a type-object invocant';

# :D: method with a following positional param
class E {
    method f(E:D: $x) { $x * 2 }
}
is E.new.f(21), 42, ':D: invocant plus a positional param';

# :U:/:D: combined with a return type
class F {
    multi method label(F:U: --> Str) { 'type' }
    multi method label(F:D: --> Str) { 'instance' }
}
is F.label,     'type',     ':U: candidate with return type';
is F.new.label, 'instance', ':D: candidate with return type';

# A plain (no-smiley) anonymous invocant marker is still fine
class G {
    method h(G:) { 'ok' }
}
is G.new.h, 'ok', 'plain anonymous typed invocant marker still works';
