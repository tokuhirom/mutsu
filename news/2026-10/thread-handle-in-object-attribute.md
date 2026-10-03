# A file handle kept in an object attribute works on a worker thread

A spawned thread is given clones of the IO handles its environment
references. Only handles bound directly to a variable were found, so an object
that kept its handle in an attribute (`has IO::Handle $!log-h`) and was used
from `start { ... }` or a `start react` died with "Invalid IO::Handle". The
spawn now also collects handles held in object attributes (a few objects
deep).

Found by the ecosystem roulette on Log::Dispatch, whose `File` destination
writes from a reactor thread. The distribution's remaining failures need
#11268 (synchronous `Supplier.emit` into a `react` on another thread) and
#11269 (`$*STACK-ID`).
