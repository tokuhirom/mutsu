# HyperSeq's and RaceSeq's methods are rows, and agree wherever they are called

A hyper or race sequence's own methods — `hyper`, `race`, `configuration`, `is-lazy`, `iterator`, `map` and `grep` —
had three implementations, one per call path, and they had drifted. `my $h = (1..6).hyper(:batch(2));
$h.configuration` died with "No such method 'configuration' for invocant of type 'List'", and `$h.map(...)` ran
sequentially while `(1..6).hyper.map(...)` used the worker pool.

They are now rows of the method table with one implementation each (ADR-11276 §9.137). A bad `:batch` or `:degree`
reports Rakudo's message: `Invalid value '0' for :batch on method hyper`.
