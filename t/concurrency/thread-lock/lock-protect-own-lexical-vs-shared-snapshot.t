use Test;
plan 3;

# A thread body assigns a scalar called $name; that publishes a by-name
# snapshot of it to the shared-variable store.
{
    my $name = "thread-side";
    await start { $name = "post"; };
}

# Once threads exist, every scalar store publishes under its bare name, so an
# unrelated scope's `my $name` leaves a stale snapshot in the shared store.
sub unrelated { my $name = "post"; $name }
unrelated();

class Aliaser {
    has $!lock = Lock.new;
    method alias(Str $name = "default") {
        $!lock.protect: { "got $name" }
    }
    method alias-capture(|c (Str $name = "default")) {
        $!lock.protect: { "got $name" }
    }
}

my $a = Aliaser.new;
is $a.alias("post_author"), "got post_author", 'protect block reads its own $name parameter';
is $a.alias-capture("post_author"), "got post_author", 'also under a |c sub-signature';
sub plain(Str $name) { my $l = Lock.new; $l.protect: { $name } }
is plain("mine"), "mine", 'and in a plain sub';
