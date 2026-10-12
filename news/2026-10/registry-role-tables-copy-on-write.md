# A thread clone no longer copies every role definition

Every `start` block, `.then` callback and supply tap runs on a clone of the
interpreter that shares its parent's type registry. The first write the clone
makes to that registry copies it. A routine that declares an inner `my sub`
writes on every call, because it re-installs that sub. The copy was shallow for
the class table but deep for the role table, the role candidates, and the
per-class sub and role-parameter maps. So each such clone kept a private copy of
every role the program had loaded.

With Cro loaded, this cost about 10 MB per keep-alive connection: each
connection's pipeline holds a few such clones for as long as the connection
stays open. The four tables are now shared copy-on-write, like the class table,
and a write copies only the table it changes.

The request loop from #12981 (a fresh `Cro::HTTP::Client` per request) now
retains 4.2 MB per request instead of 13.4 MB. LLM::Chat's
`t/21-stream-encoding.rakutest` used to be OOM-killed at 13.9 GB. It now runs
to the end, peaking at 6.9 GB.
