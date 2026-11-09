# The parse-time module scan memoizes scans a `use` cycle truncated

A module that `use`s a module which loads it back (through a `require` in a routine, as
Qwiratry's `Operator::Capability` does with `Query::Runtime`) made the parse-time export
scan skip the cycle partner, and a truncated scan was never cached, so every `use` path
through the cycle re-parsed each module of it: loading `Qwiratry::Walker::Factory` took
87 s and `Qwiratry::Aggregate::Driver` 3 min 25 s. A truncated scan is now memoized with
the loading modules it skipped and reused where those are loading too, and the scan of
the module a cycle starts at (which skipped only itself) is complete, so it is cached in
process and on disk. Cold, the two load in 12 s; with the disk cache, in 0.5 s.
