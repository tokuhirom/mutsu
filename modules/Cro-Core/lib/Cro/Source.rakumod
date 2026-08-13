# A source is a source of either connections or messages. The `produces`
# method returns the type of connection or message that is produced. The
# `incoming` method provides a `Supply` that should be tapped in order to
# start the flow of data.
role Cro::Source {
    method incoming() returns Supply:D { ... }
    method produces() { ... }
}
