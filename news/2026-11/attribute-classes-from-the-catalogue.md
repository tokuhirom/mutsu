# Proc, IO::Handle, the Systemic classes and Pod are built on first lookup

`build_builtin_registry` used to build 26 classes eagerly. These were the built-in classes
with attributes:

- `Proc`, `Proc::Async::Pipe`
- `IO::Handle`, `IO::CatHandle` and its clone `IO::ArgFiles`
- `Distro`, `Kernel`, `VM`, `Compiler`, `Raku`/`Perl`
- `Encoding::Builtin`
- the Pod object model (`Pod::Block` and the classes derived from it)
- the IO notification classes

It also wrote each class's accessor rows into the method table. That was about half of what
was left of the registry build.

The code that built them moved verbatim into `builtin_class_catalogue_attr::build_all`
(ADR-19206). It now runs once per process, the first time any of these classes is looked up.
The classes became the static class catalogue's third row segment. The registry's class table
now starts empty, so every built-in class is a catalogue row.

Their accessor rows are no longer synced at start-up. `Registry::accessor_is_public_sym`
answers a built-in class's accessors from its own attribute list. It already did this for the
built-in exception classes (#18956). A class the program redeclares or augments still gets its
rows synced, as any user class does.

Measured on `benchmarks/bench-startup.raku` (`MUTSU_JIT=off`, CI-style release build) against
`main` after #19231:

| | before | after |
|---|---:|---:|
| instructions | 4.08M | 3.57M (-12.3%) |
| `build_builtin_registry` instructions | 0.71M | 0.22M |
| minor faults | ~973 | ~934 |
| median wall time | 5.33 / 5.62 ms | 5.11 / 5.23 ms |
