# Classify Interpreter field allowances by subsystem

The `Interpreter` direct-field ratchet now reads one allowance file per
subsystem. The frozen 439-name baseline is gone; every current field has an
explicit subsystem owner. Future subsystem extractions add their
holder field to that subsystem's file, while names of extracted fields can
remain until cleanup. The rule against adding unreviewed direct fields is
unchanged.
