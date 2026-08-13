use Log::Timeline::Output;
use JSON::Fast;

# The protocol version we understand (adapt when we have multiple).
my constant VERSION = 1;

#| Sends output over a socket.
class Log::Timeline::Output::Socket does Log::Timeline::Output {
    #| The host to listen on.
    has Str $.host = 'localhost';

    #| The port to listen for connections on.
    has Int $.port is required;

    #| Channel for sending events.
    has Channel $!events .= new;

    #| Promise kept when we need to start closing.
    has $!closing = Promise.new;

    #| Promise kept once we have shut down, which corresponds to the reactor for
    #| processing events terminating.
    has $!reactor-done = self!start-reactor.Promise;

    #| The reactor starts the server, listens for events that we should log, and
    #| sends them. It also saves events up until the initial connection. It is
    #| assumed there will be a single active connection in most use cases.
    method !start-reactor() {
        supply {
            my %connections{IO::Socket::Async} is Hash;
            my @unsent;

            whenever IO::Socket::Async.listen($!host, $!port, :LOG-TIMELINE-IGNORE) -> $conn {
                my $handshook = False;
                whenever $conn.Supply.lines -> $message {
                    unless $handshook {
                        # Check handshake message.
                        my %init := from-json $message;
                        if %init<min> ~~ Int && %init<max> ~~ Int {
                            if %init<min> <= VERSION <= %init<max> {
                                %connections{$conn} = True;
                                accept-connection($conn);
                            }
                            else {
                                error($conn, "Unsupported version");
                            }
                        }
                        else {
                            error($conn, "Missing min/max versions in handshake");
                        }
                        CATCH {
                            default {
                                error($conn, "Invalid handshake");
                            }
                        }
                    }
                    LAST %connections{$conn}:delete;
                }
            }

            sub error($conn, $err) {
                my $json = to-json :!pretty, { :$err }
                whenever $conn.print("$json\n") {
                    $conn.close;
                }
            }

            sub accept-connection($conn) {
                my $handshake-json = to-json :!pretty, { :ver(VERSION) }
                $conn.print("$handshake-json\n");
                while @unsent.shift -> $event-json {
                    $conn.print("$event-json\n");
                }
            }

            whenever $!events -> $event-json {
                if %connections {
                    .print("$event-json\n") for %connections.keys;
                }
                else {
                    push @unsent, $event-json;
                }
            }

            whenever $!closing {
                .close for %connections.keys;
                done;
            }
        }
    }

    #| Logs an event.
    method log-event($type, Int $parent-id, Instant $timestamp, %data --> Nil) {
        $!events.send: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(0),
            :p($parent-id), :t($timestamp.Rat), :d(%data)
        }
    }

    #| Logs the start of a task.
    method log-start($type, Int $parent-id, Int $id, Instant $timestamp, %data --> Nil) {
        $!events.send: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(1),
            :i($id), :p($parent-id), :t($timestamp.Rat), :d(%data)
        }
    }

    #| Logs the end of a task.
    method log-end($type, Int $id, Instant $timestamp --> Nil) {
        $!events.send: to-json :!pretty, {
            :m($type.module), :c($type.category), :n($type.name), :k(2),
            :i($id), :t($timestamp.Rat)
        }
    }

    #| Close the socket, once all outstanding events are sent.
    method close(--> Nil) {
        $!closing.keep;
        await $!reactor-done;
    }
}
