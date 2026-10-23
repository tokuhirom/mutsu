# A closure in a run-time added method captures the creator's sigilless parameters

A method added with `^add_multi_method` (or `^add_method`) is re-compiled from
its source when first called, with a fresh compiler that did not know the
sigilless parameters (`\type`) of the routine that built it. A closure created
in its body therefore compiled a bare `type` as a bareword, captured nothing,
and read whatever `type` the interpreter env held when it finally ran: Red's
comparate accessors (`Proxy.new: FETCH => -> $ { ... type.^columns ... }`) died
with "No such method 'columns' for invocant of type ...ClassHOW". The
re-compile now seeds the captured env's sigilless names, as the fallback
routine path already does (#14094).
