use Test;

# A closure passed to `Promise.then` runs later on the thread pool when the
# promise is kept, so it escapes the call frame. A lexical it captures which
# the parent reassigns *after* registering the continuation must be a shared
# cell, so the continuation observes the new value rather than a stale
# clone-time snapshot. Same class as the `.tap`/`.act` escape fix.

plan 1;

my $gate;
my $p = Promise.new;
my $seen;
my $done = $p.then(-> $x {
    $seen = $gate.defined ?? $gate.status.Str !! "undef";
});
$gate = Promise.new;   # reassign AFTER .then registered
$p.keep(True);
await $done;
is $seen, 'Planned',
    '.then continuation sees the parent-reassigned lexical (shared cell)';

# vim: expandtab shiftwidth=4
