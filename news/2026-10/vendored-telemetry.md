# Rakudo's `Telemetry` module runs verbatim

`use Telemetry` now loads Rakudo's own `lib/Telemetry.rakumod`, vendored
unmodified from the 2026.06 release into `modules/Rakudo-Core` (#9824).
`T<cpu max-rss>`, `snap(@t)`, `periods(@t)` and `report(@t)` work, and
rakudo's `t/06-telemetry` usage, thread and thread-pool suites pass.

Getting the real module to run took several interpreter fixes, none of them
specific to it:

- `Thread.usage` and `$*SCHEDULER.usage` report real counters (threads
  started, completed, aborted, joined, yields, highest id; pool workers,
  queued and completed tasks) as native int rows, and the type objects
  `Kernel`, `Thread` and `ThreadPoolScheduler` answer the methods rakudo
  implements without an instance.
- `Rakudo::Internals.INITTIME` gives the process start time.
- `nqp::create(Rakudo::Internals::IterationSet)` builds a VM hash.
- Assigning a native `str` to a native `int` variable coerces it, as rakudo's
  native semantics do.
- A term followed directly by `<...>` (`T<cpu>`, `f<key>`) is a call and then a
  subscript, not a listop with a word list.
- A hyper subscript (`>>.[1]`, `».{$k}`, `>>.<k>`) continues an interpolated
  expression, as in `"%format{@columns}>>.[HEADER].join(' ')"`.

The default-snapshot form (`snap; ...; LAST snap; report`) still misreports,
because rebinding a module-level variable also moves an earlier `:=` alias of
it; that is tracked in #11797.
