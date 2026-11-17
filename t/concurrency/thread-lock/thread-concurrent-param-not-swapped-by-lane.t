use Test;

# #17794 (IO::Socket::Async::SSL's ALPN result swapped between two concurrent
# connects): once any method in the program wrote a lexical named `$alpn`, the
# name rode the name-keyed cross-thread lane, so a closure created inside a
# `start` read whichever concurrent invocation's `$alpn` was published last --
# and even the file-scope `$alpn` was overwritten. Since ADR-16595 slice 5 a
# parameter is reached only through its own invocation's cell.

plan 3;

my $lock = Lock.new;
my $alpn = 'file';

class Conn {
    method connect(:$alpn) { self!setup(:$alpn) }
    method !setup(:$alpn) {
        start {
            await Promise.in(0.05);
            $lock.protect: { $alpn.join(',') }
        }
    }
}

# Any method writing a lexical called `$alpn` used to put the name on the lane.
class Writer { method w { $alpn = 'written' } }

my @r = await (start { await Conn.connect(alpn => <http/1.1>) }),
              (start { await Conn.connect(alpn => <h2 http/1.1>) });
is @r[0], 'http/1.1', 'the first invocation reads its own parameter';
is @r[1], 'h2,http/1.1', 'the second invocation reads its own parameter';
is $alpn, 'file', 'the file-scope lexical of the same name is untouched';
