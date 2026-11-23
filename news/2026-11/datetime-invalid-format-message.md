# An invalid DateTime string reads as rakudo's

`DateTime.new("garbage")` dies with rakudo's `X::Temporal::InvalidFormat`. Its
message is built from the exception's attributes:

```
Invalid DateTime string 'garbage'; use an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00) instead
```

mutsu printed `use yyyy-mm-ddThh:mm:ss±hhmm or bg an abbreviation`, and the
exception had no `.format`. `"garbage".DateTime` raised the Date flavour
("Invalid Date string ...; use yyyy-mm-dd instead"), because a string without
a `T` was parsed as a date. The exception now carries `format` and builds its
message the way rakudo does. `Str.DateTime` uses the DateTime parser, which
already reads a date-only string as midnight UTC.
