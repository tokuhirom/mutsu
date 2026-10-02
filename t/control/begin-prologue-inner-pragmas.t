use Test;

# ADR-0134 slice 2 (#10472): a BEGIN nested in a scope that declares an
# operator code variable or a lexical pragma ahead of it is still lifted to
# BEGIN time. It runs once, before the unit's run time, whether or not the
# enclosing scope ever runs. The operator variable holds its static value
# there, and the pragma is in effect.

BEGIN plan 11;

my @log;

sub after-op-var { my &infix:<xx2> = { $^a ~ $^b }; BEGIN @log.push('op-var') }
sub after-use-strict { use strict; BEGIN @log.push('use strict') }
sub after-no-strict { no strict; BEGIN @log.push('no strict') }
sub after-fatal { use fatal; BEGIN @log.push('fatal') }
sub after-newline { use newline :lf; BEGIN @log.push('newline') }
sub after-variables { use variables :D; BEGIN @log.push('variables') }
sub after-unused-decl { my Int $x = 1; use variables :D; BEGIN @log.push('decl, variables') }
is @log.head(7).join(','),
    'op-var,use strict,no strict,fatal,newline,variables,decl, variables',
    'a BEGIN after an operator variable or a pragma runs though its scope never does';

my @later;
sub first { use strict; BEGIN @later.push(1) }
sub second { BEGIN @later.push(2) }
is @later.join(','), '1,2', 'a BEGIN after a pragma does not keep later BEGINs from running';

sub static-op { my &infix:<yy> = { $^a ~ '|' ~ $^b }; my $v = BEGIN &infix:<yy>.defined; ($v, 1 yy 2) }
is-deeply static-op(), (False, '1|2'), 'the operator variable is unassigned at BEGIN time';
is-deeply static-op(), (False, '1|2'), '... and its initializer still runs on every entry';

sub begin-assigns { my &infix:<zz>; BEGIN &infix:<zz> = { $^a * $^b }; 3 zz 4 }
is begin-assigns(), 12, 'a BEGIN assigns an operator variable of the scope';
is begin-assigns(), 12, '... which every entry of the scope starts from';

my $used;
sub begin-uses { my &infix:<pp>; BEGIN &infix:<pp> = { $^a + $^b }; BEGIN $used = 2 pp 3 }
is $used, 5, 'a BEGIN uses an operator a BEGIN before it assigned';

sub symbolic { my &infix:<ww> = { $^a - $^b }; BEGIN @log.push('symbolic'); &::('infix:<ww>')(9, 2) }
is symbolic(), 7, 'the operator variable is still reached by symbolic lookup in place';

my $fatal;
sub under-fatal { use fatal; BEGIN $fatal = (try { my $r = (sub { fail 'x' })(); 'returned' }) // 'thrown' }
is $fatal, 'thrown', 'use fatal is in effect in the BEGIN';

my $failure = (sub { fail 'x' })();
ok $failure ~~ Failure, '... and not after it';
$failure.so;

is @log.elems + @later.elems, 10, 'every BEGIN ran once';
