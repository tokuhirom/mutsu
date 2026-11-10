use Test;

plan 2;

# A callee's `my $value = $data` (an aggregate shared into a scalar) must not
# promote slot 0 of a method caller: `self` stayed the class.
sub resolve($data) { my $value = $data; $value = $value<kind>; $value }
my $cb = sub ($av) { my $r = resolve($av); return };

class V {
    method validate($data is copy) {
        $cb($data);
        self.^name
    }
}

is V.new.validate({ kind => 1 }), 'V', 'self survives a callee sharing a same-named aggregate param';
is V.new.validate({ kind => 2, other => 3 }), 'V', 'and on a second call';

# vim: expandtab shiftwidth=4
