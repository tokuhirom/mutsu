# Capture.Hash returns a mutable Hash

`Capture.Hash` used to share `.hash`'s immutable `Map`, so `.Capture.Hash<k>:delete` died with
"Can not remove values from a Map". `.Hash` now builds a real `Hash`; `.hash` stays a `Map`, as in
Rakudo. Found through the Proc::Q suite (`t/01-basic.t`).
