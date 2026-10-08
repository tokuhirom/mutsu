use Test;

plan 3;

# A mainline ENTER sees an `our` container declared later in the unit:
# rakudo installs the symbol at compile time.
our @log;
ENTER { @log.push("enter") }
@log.push("body");
is-deeply @log.List, ("enter", "body"), 'ENTER pushes into a later `our @` array';

our %seen;
ENTER { %seen<enter> = 1 }
%seen<body> = 1;
is-deeply %seen.keys.sort.List, ("body", "enter"), 'ENTER writes a later `our %` hash';

our $n;
ENTER { $n = 5 }
is $n, 5, 'ENTER assigns a later `our $` scalar';
