use Test;

# ADR-16595 rule 1: an `@`/`%` that an escaping closure captures gets its
# declaration-site cell even when the owning frame never writes it (the ADR-0055
# vouch). A `tap` or `.then` callback made before any thread exists then reaches
# the container through that cell from whatever thread runs it, not through
# the name-keyed lane, which an unrelated same-named `@log` publishes too.

plan 6;

sub other() { my @log = <OTHER>; await start { 1 }; @log.join }

sub tap-push() {
    my @log;
    my $go = Promise.new;
    my $done = Promise.new;
    my $s = supply { whenever $go { emit 'x' } };
    $s.tap: -> $v { @log.push($v); $done.keep };
    other();
    start { sleep 0.05; $go.keep };
    await Promise.anyof($done, Promise.in(10));
    @log.join(",")
}
is tap-push(), "x", "a tap callback's push reaches the owner's vouched \@log";
is other(), "OTHER", "the unrelated same-named \@log is untouched";

sub then-replace() {
    my @log = <old>;
    my $go = Promise.new;
    my $done = Promise.new;
    $go.then({ @log = <new>, 'x'; $done.keep });
    other();
    start { $go.keep };
    await Promise.anyof($done, Promise.in(10));
    @log.join(",")
}
is then-replace(), "new,x", "a .then callback's whole assignment replaces the owner's \@log";

sub then-hash() {
    my %seen;
    my $go = Promise.new;
    my $done = Promise.new;
    $go.then({ %seen<k> = 'v'; $done.keep });
    start { $go.keep };
    await Promise.anyof($done, Promise.in(10));
    %seen.kv.join("=")
}
is then-hash(), "k=v", "a .then callback's key store reaches the owner's vouched \%seen";

# Two concurrent calls each keep their own vouched container.
sub per-call($k) {
    my @mine;
    my $done = Promise.new;
    Promise.in(0.02).then({ @mine.push($k); $done.keep });
    await Promise.anyof($done, Promise.in(10));
    @mine.join
}
my @r = await (^4).map: -> $k { start { per-call($k) } };
is @r.join(","), "0,1,2,3", "concurrent calls' callbacks reach their own call's \@mine";

# The callback runs after the owning frame returned.
sub late() {
    my @acc;
    my $go = Promise.new;
    my $p = $go.then({ @acc.push('late'); @acc.elems });
    ($go, $p, -> { @acc.join })
}
my ($go, $p, &peek) = late();
$go.keep;
await $p;
is peek(), "late", "a callback that runs after its frame returned still reaches the container";
