use Cro::Connection;
use Cro::Connector;
use Cro::Message;
use Cro::Replyable;
use Cro::Sink;
use Cro::Source;
use Cro::Types;
use Cro::TCP::NoDelay;

class Cro::TCP::Message does Cro::Message {
    has Blob $.data is rw;
    has $.connection;

    method trace-output(Cro::TCP::Message:D:) {
        "TCP Message\n" ~ self!trace-blob($!data).indent(2)
    }
}

class Cro::TCP::Replier does Cro::Sink {
    has $!socket;
    
    submethod BUILD(:$!socket!) { }
    
    method consumes() { Cro::TCP::Message }

    method sinker(Supply:D $pipeline) returns Supply:D {
        supply {
            whenever $pipeline {
                whenever $!socket.write(.data) {}
            }
            CLOSE $!socket.close;
        }
    }
}

class Cro::TCP::ServerConnection does Cro::Connection does Cro::Replyable {
    has $!socket;
    has $.replier;

    method socket-host() { $!socket.socket-host }
    method socket-port() { $!socket.socket-port }
    method peer-host()   { $!socket.peer-host }
    method peer-port()   { $!socket.peer-port }

    method produces() { Cro::TCP::Message }

    submethod BUILD(:$!socket!) {
        $!replier = Cro::TCP::Replier.new(:$!socket)
    }

    method incoming() {
        supply {
            whenever $!socket.Supply(:bin) -> $data {
                emit Cro::TCP::Message.new(:$data, connection => $!socket);
            }
        }
    }
}

class Cro::TCP::Listener does Cro::Source {
    has Str $.host = 'localhost';
    has Cro::Port $.port is required;
    has Bool $.nodelay = False;

    method produces() { Cro::TCP::ServerConnection }

    method incoming() {
        supply {
            whenever IO::Socket::Async.listen($!host, $!port) -> $socket {
                nodelay($socket) if $.nodelay;
                emit Cro::TCP::ServerConnection.new(:$socket);
            }
        }
    }
}

class Cro::TCP::Connector does Cro::Connector {
    class Transform does Cro::Transform {
        has $!socket;

        submethod BUILD(IO::Socket::Async :$!socket!) {}

        method consumes() { Cro::TCP::Message }
        method produces() { Cro::TCP::Message }

        method transformer(Supply $incoming --> Supply) {
            supply {
                whenever $incoming {
                    whenever $!socket.write(.data) {}
                }
                whenever $!socket.Supply(:bin) -> $data {
                    emit Cro::TCP::Message.new(:$data);
                    LAST done;
                }
                CLOSE {
                    $!socket.close;
                }
            }
        }
    }

    method consumes() { Cro::TCP::Message }
    method produces() { Cro::TCP::Message }

    method connect(:$nodelay, *%options --> Promise) {
        IO::Socket::Async.connect(%options<host> // 'localhost', %options<port>)
            .then: {
                my $socket = .result;
                nodelay($socket) if $nodelay;
                Transform.new(:$socket)
            }
    }
}
