use Cro::Message;

role Cro::Connection {
    method produces() returns Cro::Message:U { ... }
    method incoming() returns Supply:D { ... }
}
