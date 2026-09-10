# Test owns the unqualified `skip` name after it is imported.  The core list
# routine is therefore tested separately in core-skip-list.t.
use Test;

plan 1;
skip "deliberately skipped", 1;
