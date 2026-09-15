# Restore both dynamic output aliases after `temp`

`temp $*OUT = $handle` now restores both the sigil-less dynamic binding and
the output-routing `$*OUT` alias when its scope exits. Closing the temporary
handle therefore no longer makes later `say` calls write to a closed handle.

The regression was found while running App::ByWord 0.0.5's test suite.
