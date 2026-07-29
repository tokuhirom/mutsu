unit class RequiredDriver;
use RequiredDriver::Native;

# A file-scope `constant` the module's own routines close over, but which nothing
# exports. Same lifetime problem as the imported type name below: it is declared
# in the env of whatever frame loaded the module.
constant SLOT-SIZE = 7 * 6;

# A file-scope `my` hash, the sigiled twin of the constant above.
my %slot-names = big => 'wide', small => 'narrow';

has $.parent;

# All of these resolve names the module declared or imported for itself, long
# after whatever frame ran the `require` has returned.
method widget-name(--> Str) { Widget.^name }
method widget-label(--> Str) { Widget.new.label }
method widget-tag(--> Str) { WIDGET-TAG }
method slot-size(--> Int) { SLOT-SIZE }
method slot-size-via-sub(--> Int) { slot-size-helper() }
method slot-name(Str $key --> Str) { %slot-names{$key} }
method kind-of(Str $key --> Str) { kind-of($key) }
method kind-count(--> Int) { kind-count() }
method kind-at(Int $i --> Str) { kind-at($i) }

sub slot-size-helper(--> Int) { SLOT-SIZE + 1 }
