use Test;
use nqp;

# The socket ops of the Asynchronous nqp:: family push `[schedulee,
# args...]` onto a ConcBlockingQueue (#11502, ADR-11502 §2.4.4). Raw items
# are read inline: a VMArray passed to a Raku sub is hllized.

plan 23;

my class Q is repr('ConcBlockingQueue') {}
my class TC is repr('AsyncTask') {}

my $q := nqp::create(Q);
my $sentinel := nqp::timer($q, nqp::hash(), 10000, 0, TC);

# Listening.
my $lh := nqp::asynclisten($q, 'listen', '127.0.0.1', 0, 128, TC);
is $lh.^name, 'TC', 'asynclisten answers a handle of the given type';
my $item := nqp::shift($q);
is nqp::atpos($item, 0), 'listen', 'once listening, it pushes the schedulee';
nok nqp::isconcrete(nqp::atpos($item, 1)), 'with no connection';
is nqp::atpos($item, 5).^name, 'BOOTIO', 'the server handle';
is nqp::unbox_s(nqp::atpos($item, 6)), '127.0.0.1', 'the bound host';
my $port = nqp::unbox_i(nqp::atpos($item, 7));
ok $port > 0, 'and the port the OS picked';

# Connecting: the listener and the connect each push one item.
my $ch := nqp::asyncconnect($q, 'connect', '127.0.0.1', $port, TC);
is $ch.^name, 'TC', 'asyncconnect answers a handle of the given type';
my $server-conn;
my $client-conn;
my $client-local-port;
for ^2 {
    my $it := nqp::shift($q);
    if nqp::ishash($it) { last }
    if nqp::atpos($it, 0) eq 'listen' {
        $server-conn := nqp::atpos($it, 1);
    }
    else {
        $client-conn := nqp::atpos($it, 1);
        is nqp::unbox_s(nqp::atpos($it, 3)), '127.0.0.1', 'the connect reports the peer host';
        is nqp::unbox_i(nqp::atpos($it, 4)), $port, 'and the peer port';
        $client-local-port = nqp::unbox_i(nqp::atpos($it, 6));
    }
}
is $server-conn.^name, 'BOOTIO', 'the listener pushes the accepted connection';
is $client-conn.^name, 'BOOTIO', 'the connect pushes its connection';
ok $client-local-port > 0, 'with its local port';

# Writing and reading.
nqp::asyncreadbytes($server-conn, $q, 'read', buf8.new, TC);
nqp::asyncwritebytes($client-conn, $q, 'wrote', "hello".encode, TC);
my $got = '';
my $wrote;
my $eof = False;
my $closed = False;
until $eof {
    my $it := nqp::shift($q);
    if nqp::ishash($it) { last }
    my $name = nqp::atpos($it, 0);
    if $name eq 'wrote' {
        $wrote = nqp::unbox_i(nqp::atpos($it, 1));
        nok nqp::isconcrete(nqp::atpos($it, 2)), 'the write reports no error';
        is nqp::closefh($client-conn).^name, 'BOOTIO', 'closefh answers the handle';
        $closed = True;
    }
    elsif $name eq 'read' {
        my $data := nqp::atpos($it, 2);
        if nqp::isconcrete($data) {
            is $data.^name, 'Buf[uint8]', 'a chunk is a Buf of the given type';
            $got ~= $data.decode;
        }
        else {
            nok nqp::isconcrete(nqp::atpos($it, 3)), 'the end of the stream has no error';
            $eof = True;
        }
    }
}
is $wrote, 5, 'the write reports the bytes written';
is $got, 'hello', 'the reader gets them';
ok $closed && $eof, 'closing the client ends the server read';

# A refused connection.
nqp::asyncconnect($q, 'refused', '127.0.0.1', $port, TC) if nqp::cancel($lh);
# The listener may still drain one backlogged connection after its cancel,
# so this checks only that the connect answers.
my $it := nqp::shift($q);
ok nqp::atpos($it, 0) eq 'refused' | 'listen', 'a connect after the cancel answers';

throws-like { nqp::asyncconnect(42, 's', '127.0.0.1', 1, TC) }, Exception,
    message => 'asyncconnect target queue must have ConcBlockingQueue REPR', 'the queue is checked';
throws-like { nqp::asyncreadbytes(42, $q, 's', buf8.new, TC) }, Exception,
    message => 'read bytes asynchronously requires an object with REPR MVMOSHandle (got Int with REPR P6opaque)',
    'asyncreadbytes refuses a non-handle';
throws-like { nqp::asyncconnect($q, 's', 'no.such.host.invalid', 80, TC) }, Exception,
    message => /^"Failed to resolve host name 'no.such.host.invalid'"/,
    'an unresolvable host dies at once';
nqp::cancel($sentinel);
