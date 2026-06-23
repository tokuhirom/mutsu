use Test;

# Pin for the §D ③ ctor fork: native-ized `.new` for the clean static built-in
# constructors FakeScheduler / Proxy / Match. These are pure data assembly (or a
# process-global counter for FakeScheduler) — no env / registry / user code — so
# the VM builds them directly via `try_native_builtin_construct`, byte-identical
# to the interpreter's `dispatch_new` arm (both call the same per-type helper).

plan 14;

# --- FakeScheduler -----------------------------------------------------------
my $sched = FakeScheduler.new;
isa-ok $sched, FakeScheduler, 'FakeScheduler.new yields a FakeScheduler';
my $sched2 = FakeScheduler.new;
isa-ok $sched2, FakeScheduler, 'a second FakeScheduler.new also works';

# --- Proxy -------------------------------------------------------------------
my $backing = 10;
my $p := Proxy.new(
    FETCH => method ()   { $backing      },
    STORE => method ($v) { $backing = $v * 2 },
);
is $p, 10, 'Proxy FETCH reads the backing value';
$p = 5;
is $backing, 10, 'Proxy STORE ran the storer (5 * 2)';
is $p, 10, 'Proxy FETCH reflects the stored value';

# A Proxy with only FETCH (read-only-ish view)
my $ro := Proxy.new(FETCH => method () { 42 }, STORE => method ($v) { });
is $ro, 42, 'FETCH-only Proxy returns its computed value';

# --- Match -------------------------------------------------------------------
my $m = Match.new(:orig("hello world"), :from(0), :pos(5));
isa-ok $m, Match, 'Match.new yields a Match';
is $m.Str, 'hello', 'Match matched substring is orig[from..pos]';
is $m.from, 0, 'Match .from';
is $m.to, 5, 'Match .to';
is $m.orig, 'hello world', 'Match .orig';

my $m2 = Match.new(:orig("abcdef"), :from(2), :to(5));
is $m2.Str, 'cde', 'Match honours :to as well as :pos';

# Match with positional captures
my $m3 = Match.new(:orig("xy"), :from(0), :pos(2), :list(["a", "b"]));
is $m3.list.elems, 2, 'Match positional captures stored';

# Match with named captures
my $m4 = Match.new(:orig("xy"), :from(0), :pos(2), :hash({k => "v"}));
is $m4.hash<k>, 'v', 'Match named captures stored';
