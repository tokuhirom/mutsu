use Test;

plan 10;

my $x = 42;
is MY::(q[$x]), 42, 'bare MY symbolic deref reads a lexical scalar';
is LEXICAL::(q[$x]), 42, 'bare LEXICAL symbolic deref reads a lexical scalar';
is MY::('x').exception.^name, 'X::NoSuchSymbol',
    'bare MY symbolic deref reports a missing sigilless symbol';

our $global-value = 17;
is GLOBAL::(q[$global-value]), 17, 'bare GLOBAL symbolic deref reads a global';
dies-ok { GLOBAL::('Int') }, 'missing bare package symbol throws';

sub twice($value) { $value * 2 }
is MY::{'&twice'}(3), 6, 'MY stash contains a registered routine';
is LEXICAL::{'&twice'}(4), 8, 'LEXICAL stash contains a registered routine';
ok MY::{'&twice'} === &twice, 'MY stash contains the routine object';

{
    sub inner($value) { $value + 1 }
    is MY::{'&inner'}(9), 10, 'MY stash contains a block-local routine';
}
ok !MY::{'&inner'}.defined, 'block-local routine leaves MY stash with its scope';
