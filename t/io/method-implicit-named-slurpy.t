use Test;

plan 7;

# Every method exposes an implicit `*%_` slurpy: a Hash of the named arguments
# not bound to an explicit named parameter, and an *empty* Hash when none were
# passed. Previously, when a method was called with only positionals (and so hit
# the fast dispatch path), `%_` read as `Any` instead of `{}`, so forwarding
# `|%_` splatted a stray positional and constructors failed with
# "only takes named arguments". This blocked DBIish (DBIish.install-driver
# forwards `|%_` from a method called with a single positional).

class C { has $.parent; has $.x; }

class Maker {
    method build($driver) {
        # %_ here is empty (no named args), so |%_ adds nothing.
        return C.new(:parent($driver), |%_);
    }
    method build-with-named($driver) {
        return C.new(:parent($driver), |%_);
    }
}

my $m = Maker.new;

is $m.build('P').parent, 'P', '|%_ with empty %_ forwards nothing (positional-only call)';

my $c = $m.build-with-named('P', x => 42);
is $c.parent, 'P', 'explicit named arg still binds';
is $c.x, 42, 'extra named arg forwarded through |%_';

# Direct %_ inspection
class Inspect {
    method m($pos) { %_ }
}
is Inspect.new.m('a').elems, 0, '%_ is an empty Hash for a positional-only call';
is Inspect.new.m('a', foo => 1).elems, 1, '%_ holds one leftover named arg';
ok Inspect.new.m('a') ~~ Hash, '%_ is a Hash, not Any';
is Inspect.new.m('a', foo => 1, bar => 2)<bar>, 2, '%_ keys are accessible';
