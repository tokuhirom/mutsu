use Test;

plan 3;

# `self` is lexical: a whenever body must resolve `$.attr` against the object
# whose method created it, no matter which env the native dispatch (supplier
# tap, in-process connect) happens to run under. The merge_all env merge in
# call_sub_value used don't-overwrite for `self`, so once another object's
# closure had leaked its `self` into the main env, the NEXT listener's
# whenever body read `$.attr` off the wrong object and died — swallowed, so
# the test just hung (Cro::TCP tcp.rakutest subtest 2, "$.nodelay" read off a
# Cro::TCP::Replier).

sub free-port() {
    my $t = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $c { $c.close });
    my $p = await $t.socket-port;
    $t.close;
    $p
}

class Lis {
    has Str $.tag is required;
    has Int $.port is required;
    method incoming() {
        supply {
            whenever IO::Socket::Async.listen('127.0.0.1', $!port) -> $socket {
                emit $.tag;
            }
        }
    }
}

class Polluter {
    has $.x = 'wrong-self';
    method run($trigger) {
        supply {
            whenever $trigger {
                # the nested whenever-over-Promise is what leaks this
                # method's `self` into the dispatching env
                whenever Promise.kept('k') { }
                emit $.x;
            }
        }
    }
}

my $p1 = free-port();
my $l1 = Lis.new(tag => 'one', port => $p1);
my $c1 = Channel.new;
my $t1 = $l1.incoming.tap({ $c1.send($_) });
my $cl1 = await IO::Socket::Async.connect('127.0.0.1', $p1);
is $c1.receive, 'one', 'first listener whenever body reads its own $.tag';
$cl1.close;
$t1.close;

my $sup = Supplier.new;
my @polluted;
Polluter.new.run($sup.Supply).tap({ @polluted.push($_) });
$sup.emit('go');
is @polluted, ['wrong-self'], 'polluter supply ran (leaking its self)';

my $p2 = free-port();
my $l2 = Lis.new(tag => 'two', port => $p2);
my $c2 = Channel.new;
my $t2 = $l2.incoming.tap({ $c2.send($_) });
my $cl2 = await IO::Socket::Async.connect('127.0.0.1', $p2);
is $c2.receive, 'two', 'second listener whenever body still reads ITS $.tag, not the leaked self';
$cl2.close;
$t2.close;
