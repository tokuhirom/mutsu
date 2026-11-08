use Test;
use experimental :rakuast;

plan 7;

# `$*CU` and `add-check-code` address the unit being compiled: inside an EVAL
# that is the EVAL's own unit, so check code queued in its BEGIN runs at its
# CHECK phase, whether the unit came from a string or from a RakuAST tree.

my @log;
EVAL Q|BEGIN { $*CU.add-check-code({ @log.push('string check') }) }; @log.push('string main');|;
is-deeply @log, ['string check', 'string main'], 'string EVAL: check code runs before the mainline';

@log = ();
EVAL Q|BEGIN { $*CU.add-check-code({ @log.push('first') }); $*CU.add-check-code({ @log.push('second') }) }; @log.push('main');|;
is-deeply @log, ['second', 'first', 'main'], 'string EVAL: last queued runs first';

@log = ();
my $ast = Q|BEGIN { $*CU.add-check-code({ @log.push('tree check') }) }; @log.push('tree main');|.AST;
EVAL $ast;
is-deeply @log, ['tree check', 'tree main'], 'tree EVAL: check code runs before the mainline';

@log = ();
EVAL Q|BEGIN { $*CU.add-check-code({ @log.push('check code') }) }; CHECK { @log.push('CHECK phaser') }; @log.push('main');|;
is-deeply @log, ['CHECK phaser', 'check code', 'main'], 'check code runs after the CHECK phasers';

my $seen;
EVAL Q|BEGIN { $*CU.add-check-code({ $seen = $*CU.^name }) }|;
is $seen, 'RakuAST::CompUnit', '$*CU inside the check code is a CompUnit';

my $count;
EVAL Q|BEGIN { $*CU.add-check-code({ $count = $*CU.statement-list.statements.elems }) }; 1; 2;|;
is $count, 3, 'string EVAL: $*CU is the EVAL unit, not the caller';

$count = Nil;
EVAL Q|BEGIN { $*CU.add-check-code({ $count = $*CU.statement-list.statements.elems }) }; 1; 2;|.AST;
is $count, 3, 'tree EVAL: $*CU is the EVAL unit, not the caller';
