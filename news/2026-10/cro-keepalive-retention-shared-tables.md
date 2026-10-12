# A Cro keep-alive request retains under 3 MB

Each request made through a fresh `Cro::HTTP::Client` keeps its keep-alive
connection, and with it the thread interpreters of that connection's supply
pipeline. On mutsu every such request retained 13.4 MB of RSS a day ago and
4.2 MB after the role tables became copy-on-write (#12988). It now retains
about 2.8 MB (rakudo: 1.4 MB), which meets #12981's close condition; the fd
growth was already down to rakudo's +2 per request (#12995).

The remaining copies were tables a thread interpreter cloned whole, and kept,
the first time it wrote to them:

- **A role attribute's default captured the role scope's lexicals as a plain
  map** in its `ClassAttributeDef`. That def is cloned per consuming class,
  per construction plan and per construction, so every clone deep-copied the
  role scope (15 MB at load time alone with Cro). The capture is now one shared
  `Arc`.
- **The type metadata table** (`^language-revision`, `^rw`, `^set_name`, ...)
  held each type's map inline, so registering a `my enum` in a supply body —
  Cro's request and response parsers declare one, and a thread clone registers
  it again on every connection — copied every type's metadata. Each type's map
  is now shared behind its own `Arc`, and storing an unchanged revision writes
  nothing.
- **Announcing a function-resolution change** (`renew_version`) on a function
  table another interpreter still shared deep-copied the whole function map,
  once per supply block body run. The map now sits behind its own `Arc`, so
  renewing the stamp copies only the stamp.

Separately, `is_builtin_type_method` built a native `Method` object (with a
`Signature` registered for good in a process-global table) for every catalog
row of a core type and each ancestor, just to compare names. It runs whenever a
native method object is called, so `Str.^find_method('chars')` retained about
48 kB per call; it now probes the catalog row directly and retains about 1 kB,
as rakudo does.
