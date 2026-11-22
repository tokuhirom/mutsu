# A subclass of a built-in `X::` class inherits its message

`class MyErr is X::OutOfRange { }; MyErr.new(got => 3, range => "0..1").message`
died with `Stub code executed`. Rakudo's subclass inherits
`X::OutOfRange.message`. mutsu's built-in formatter
(`value::exception_message::format_exception_message`) was looked up only
under the receiver's exact class name, so a user subclass fell through to
`Exception.message`'s stub (#18730).

The message is now built by the first class in the receiver's MRO that has a
built-in format. This covers `.message` and the `.Str`/`.gist` rendering of
both thrown and unthrown exceptions. A `method message` of the class's own
still wins.
