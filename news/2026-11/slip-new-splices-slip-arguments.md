# `Slip.new` splices its Slip arguments

`Slip.new(1, Empty, 2)` kept `Empty` as an element, so Rakudo::Options' `run-parameters` handed the child process a command line full of empty entries (`Could not open .`). The constructor now splices Slip arguments (`Empty`, `slip(..)`) into the new Slip, as rakudo does; lists, arrays and Seqs stay single elements.

Rakudo::Options t/03, t/04 and t/05 now match rakudo. t/02 now fails honestly: it expects `MVM_SPESH_LOG` to make the child write a log file, which is a MoarVM feature (it passed before only because the child never started).
