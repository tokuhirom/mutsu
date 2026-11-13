use Test;

# A sigilless term bound to a not-yet-existing hash or array element denotes
# that element's (auto-vivifying) container, so it is never `=:=` to the `Any`
# it reads as -- also when it is handed on to a raw parameter (#17656).

plan 17;

sub same-as-any(Mu \c) { c =:= Any }
sub store(\c) { c = 7 }

my %h;
my \x = %h<z>;
nok x =:= Any, 'mainline term over a missing hash element is not =:= Any';
nok same-as-any(x), 'and neither is the raw parameter it is passed to';
nok %h<z>:exists, 'binding the term does not vivify the element';

sub in-routine() {
    my %g;
    my \y = %g<z>;
    my @r = y =:= Any, same-as-any(y);
    store(y);
    @r.push: %g<z>;
    @r
}
my @r = in-routine();
nok @r[0], 'term in a routine over a missing hash element is not =:= Any';
nok @r[1], 'and neither is the raw parameter it is passed to';
is @r[2], 7, 'a write through the raw parameter vivifies the element';

my @a;
my \e = @a[2];
nok e =:= Any, 'term over a missing array element is not =:= Any';
nok same-as-any(e), 'nor its raw parameter';
store(e);
is-deeply @a, [Any, Any, 7], 'a write through the raw parameter vivifies the array element';

sub array-in-routine() {
    my @b;
    my \f = @b[1];
    my $same = f =:= Any;
    store(f);
    ($same, @b)
}
my ($same, @b) := array-in-routine();
nok $same, 'term in a routine over a missing array element is not =:= Any';
is-deeply @b, [Any, 7], 'a write through it vivifies the element';

my %n;
my \t = %n<a><b>;
nok t =:= Any, 'a nested missing element is not =:= Any either';
is-deeply %n, {}, 'and binding it vivifies nothing';

# A bare value is still the value.
my \v = Any;
ok v =:= Any, 'a term bound to Any itself is =:= Any';

# A missing key of an immutable Map is a plain value (Nil): no container.
sub same-as-nil(Mu \c) { c =:= Nil }
my $map = Map.new((a => 1));
my \mk = $map<missing>;
ok mk =:= Nil, 'a term over a missing Map key is the Nil value';
ok same-as-nil(mk), 'and so is the raw parameter it is passed to';
ok same-as-nil($map<missing>), 'as is a missing Map key passed directly';
