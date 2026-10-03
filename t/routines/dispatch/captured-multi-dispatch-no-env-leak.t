# A multi taken as a value out of a package stash dispatches through its
# captured candidates; probing which candidate applies must not bind that
# candidate's parameters into the caller's frame. Found via Test::Describe:
# a re-exported `multi ok(Mu $cond, $desc = '')` called inside `subtest`
# overwrote the subtest's own `$desc`.
use Test;
use lib 't/lib';

plan 3;

use ExportStashBase;

my &chk = ExportStashBase::EXPORT::ALL::<&check>;

sub outer(&body, $desc) { body(); $desc }
is outer({ chk(1) }, 'mine'), 'mine', 'the caller keeps its own $desc';
is outer({ chk(1, 'inner') }, 'mine'), 'mine', 'also when the candidate binds $desc explicitly';
is chk(0, 'x'), 'not ok x', 'the candidate still receives its arguments';
