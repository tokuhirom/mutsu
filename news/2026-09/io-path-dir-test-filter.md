`IO::Path.dir(test => ...)` now applies regex, string, and callable tests to
each directory entry, matching Rakudo's filtering behavior.

Closes #8623.
