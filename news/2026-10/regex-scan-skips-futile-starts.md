# The regex scan skips the starts a failed leading run makes futile, and tests TAB/LF from the ASCII set

Refs #10315: a failing `/ \w+ \s \d ** 6 /` scan over a 40,937-character subject now costs about
249 instructions per character (callgrind, profiling build, second run, net of building the
subject), down from 399 after #12785 and 1,286 before it.

- `ScanPositions::advance_to` lets the scan drop the starts inside a failed start's leading run
  instead of pulling each one from the iterator only to discard it; the engine hands the run's end
  back through its scratch (`first_run`) so the run is no longer walked a second time.
- A give-back after a run whose characters all took the ASCII-set path is skipped outright when
  the following atom's set shares none of them (`rx_run_giveback_top`).
- TAB and LF (LF after anything but CR, since CR LF is one grapheme) join printable ASCII in the
  one-grapheme fast path, so the ends of lines no longer take the full class test.
