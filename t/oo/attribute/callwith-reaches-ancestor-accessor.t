use v6;
use Test;

# A user method that shadows an ancestor's auto-generated public attribute
# accessor (`has $.body = ""` in a parent, `method body(...) { ...callwith()... }`
# in the child) needs an MRO deferral frame, same as a wrapped method or a
# native base candidate: the accessor is registry metadata, not a `MethodDef`,
# so it never appeared among the dispatch candidates a single-user-candidate
# call collects, and `callwith`/`callsame`/`nextsame` silently answered Nil
# instead of reading the attribute.
#
# Found via Email::MIME's `Email::Simple` base class (from the vendored
# distribution): `Email::MIME.body` overrides the inherited `$.body` reader
# and calls `callwith()` to read the raw stored value before decoding it.

plan 4;

class HasBody {
    has $.body = "";
}

class OverridesBody is HasBody {
    method body($decorate?) {
        my $raw = callwith();
        return $decorate ?? "<$raw>" !! $raw;
    }
}

my $o = OverridesBody.new(body => 'hello');
is $o.body, 'hello', 'callwith() reaches the ancestor auto-accessor';
is $o.body(True), '<hello>', 'the overriding method still runs its own logic around it';

class OverridesBodySame is HasBody {
    method body($decorate?) {
        my $raw = callsame();
        return $decorate ?? "<$raw>" !! $raw;
    }
}
is OverridesBodySame.new(body => 'world').body, 'world',
    'callsame() reaches the ancestor auto-accessor too';

class OverridesBodyNextsame is HasBody {
    has $.seen;
    method body() {
        $!seen = nextsame();
        return $!seen;
    }
}
is OverridesBodyNextsame.new(body => 'nextsame-val').body, 'nextsame-val',
    'nextsame() reaches the ancestor auto-accessor too';
