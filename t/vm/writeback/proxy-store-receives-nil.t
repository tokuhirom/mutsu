use Test;
use lib $*PROGRAM.parent(3).add('lib');
use ProxyNilExport;

plan 9;

# Assigning Nil to a Proxy hands its STORE the Nil itself: deciding what a Nil
# means is the Proxy's business, not a container default's.
my @log;
my $v is default(Nil) = 1;
my $p := Proxy.new(
    FETCH => -> $ { $v },
    STORE => -> $, \n { @log.push: n.raku; $v = n },
);
$p = Nil;
is-deeply @log, ['Nil'], 'statement assignment to a lexical Proxy';
is $p, Nil, 'the FETCH sees the default(Nil) variable';
is-deeply ($p = Nil), Nil, 'expression assignment to a lexical Proxy';

# The same through an imported Proxy (reached by name rather than a slot).
$PX = Nil;
is-deeply @PX-LOG, ['Nil'], 'statement assignment to an imported Proxy';
is $PX, Nil, 'reads back Nil';
is-deeply ($PX = Nil), Nil, 'expression assignment yields the FETCH';
is ($PX = 42), 42, 'a defined value still goes through';
is $PX, 42, 'and is read back';
is-deeply @PX-LOG, ['Nil', 'Nil', '42'], 'STORE saw every value as written';
