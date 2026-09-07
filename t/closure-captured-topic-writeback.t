use Test;

# A closure created inside a `.map`/`.grep` block captures the block's `$_`,
# which is the SOURCE ELEMENT's own container. Calling that closure later must
# not write the calling frame's ambient topic through that captured container.
#
# The frame-exit "rejoin an rw-argument writeback to the captured cell" step in
# `call_compiled_closure_in_unit` used to run for every name sitting on the
# process-wide pending-writeback lists. `pending_caller_var_writeback` is
# retain-on-miss: a name no frame owns a local slot for -- the implicit topic
# `_` above all -- stays on it for the rest of the run. So every later closure
# call re-ran the rejoin for `_` and stored the calling frame's ambient `$_`
# (undefined inside a method) straight into the source array element.
#
# This is what broke `URI::Query`: `!value-for` builds `Proxy` readers inside
# `@!query-form.grep(...).map(...)`, and reading one key blanked out a
# query-form element, so the next `ASSIGN-KEY` died with
# "No such method 'key' for invocant of type 'Any'".

plan 4;

# Park the topic on the retain-on-miss pending list: a sigilless parameter
# aliases `$_`, and assigning through it records `_` as a writeback source that
# no frame's local slots can ever claim.
my @src = 1, 2, 3;
sub bump(\x) { x = x + 0 }
for @src { bump($_) }

class Q {
    has Pair @!qf;
    method setup(@p) { @!qf = @p }
    method keys-str()   { @!qf.map({ .defined ?? .key   !! '(undef)' }).join(',') }
    method values-str() { @!qf.map({ .defined ?? .value !! '(undef)' }).join(',') }
    method !value-for($key) {
        my $l = @!qf.grep({ .key eq $key }).map({
            my $v = .value;
            Proxy.new(
                FETCH => method () { $v },
                STORE => method ($n) { die "read-only" },
            );
        }).List;
        $l[0]
    }
    method AT-KEY($k) { self!value-for($k) }
}

my $q = Q.new;
$q.setup([ 'foo' => 'cod', 'foo' => 'trout' ]);
is $q.keys-str, 'foo,foo', 'the attribute array starts with both pairs';

is $q<foo>, 'cod', 'reading a key answers through the Proxy';

is $q.keys-str, 'foo,foo', 'the source element survives the read';
is $q.values-str, 'cod,trout', '... with its value intact';
