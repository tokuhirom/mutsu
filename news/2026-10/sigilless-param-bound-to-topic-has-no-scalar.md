# A raw `\v` bound to the topic no longer claims a Scalar container

Passing `$_` of a `for`/`map` over plain values (`for %h.keys { f($_) }`) to a
sigilless parameter boxed the topic into a shared cell, so `v =:= v."name"()`
answered False where Rakudo answers True. DB::Xoos' `gen-quote` relies on that
test to tell identifiers from bind values; mutsu emitted `SET ? = ?` and SQLite
raised "SQL logic error". The parameter now keeps the bare value when the topic
holds no container. DB::Xoos `t/03-sqlite.t` passes 16/16.
