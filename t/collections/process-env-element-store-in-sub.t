use Test;

# #16559 (found by AI::Gator): an element store into the process-level
# %*ENV from inside a sub must outlive the sub's frame, as it does in rakudo
# (one process-wide hash).

plan 12;

sub set-literal { %*ENV<MUTSU_T_LIT> = 'lit' }
set-literal();
is %*ENV<MUTSU_T_LIT>, 'lit', 'literal-key store in a sub is kept';

sub set-var($k, $v) { %*ENV{$k} = $v }
set-var('MUTSU_T_VAR', 'var');
is %*ENV<MUTSU_T_VAR>, 'var', 'variable-key store in a sub is kept';
is %*ENV<MUTSU_T_LIT>, 'lit', 'an earlier store survives a later one';

sub read-it { %*ENV<MUTSU_T_LIT> }
is read-it(), 'lit', 'another sub reads the stored key';

sub drop-it { %*ENV<MUTSU_T_LIT>:delete }
drop-it();
nok %*ENV<MUTSU_T_LIT>.defined, 'a delete in a sub is kept';
nok %*ENV<MUTSU_T_LIT>:exists, 'the deleted key no longer exists';
is %*ENV<MUTSU_T_VAR>, 'var', 'the other key is untouched by the delete';

is-deeply %*ENV<MUTSU_T_NOPE>.raku, 'Any', 'a missing key reads as Any';

sub nested-outer { nested-inner(); }
sub nested-inner { %*ENV<MUTSU_T_NEST> = 'n' }
nested-outer();
is %*ENV<MUTSU_T_NEST>, 'n', 'a store two calls deep is kept';

sub twice { %*ENV<MUTSU_T_CNT> = (%*ENV<MUTSU_T_CNT> // 0) + 1 }
twice() for ^3;
is %*ENV<MUTSU_T_CNT>, 3, 'repeated read-modify-write in subs accumulates';

sub child-sees { run($*EXECUTABLE, '-e', 'print %*ENV<MUTSU_T_VAR>', :out).out.slurp }
is child-sees(), 'var', 'a child process gets the stored key';

my $p = start { %*ENV<MUTSU_T_START> = 'st' };
await $p;
is %*ENV<MUTSU_T_START>, 'st', 'a store in a start block is visible afterwards';
