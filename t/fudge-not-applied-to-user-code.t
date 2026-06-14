use Test;

# Roast fudge directives (#?rakudo skip/todo, #?DOES, #?v6) must NOT be applied
# to ordinary user code. Without the MUTSU_FUDGE gate, a stray `#?rakudo skip`
# comment silently dropped the next statement. These tests run under the normal
# (non-roast) harness where MUTSU_FUDGE is unset, so the directives must be inert.

plan 4;

my $a = 0;
#?rakudo skip 'this is just a regular comment, not a fudge directive'
$a = 42;
is $a, 42, '#?rakudo skip does not drop the following statement';

my $b = 0;
#?rakudo todo 'regular comment'
$b = 7;
is $b, 7, '#?rakudo todo does not drop the following statement';

my @log;
#?rakudo skip 'reason'
@log.push('kept');
is-deeply @log, ['kept'], 'statement after a skip comment still runs';

# A directive-looking comment followed by more code in a block.
my $c = do {
    #?rakudo skip 'x'
    100;
};
is $c, 100, 'skip comment inside a block does not eat the value';
