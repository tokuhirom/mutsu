use Test;

# A block inlined into its enclosing code -- a bare block, an `if`/`else`,
# `when`/`default` or `try` body -- is a fresh clone each time it is entered,
# so its FIRST runs on every entry, even with no ENTER/LEAVE beside it
# (#17992). Checked against rakudo.

plan 8;

my @log;
for ^3 { if True { FIRST @log.push('if') } }
is-deeply @log, ['if', 'if', 'if'], 'an if body';

@log = ();
for ^2 { if False { @log.push('no') } else { FIRST @log.push('else') } }
is-deeply @log, ['else', 'else'], 'an else body';

@log = ();
my $i = 0;
while $i++ < 2 { { FIRST @log.push('bare') } }
is-deeply @log, ['bare', 'bare'], 'a bare block in a while loop';

@log = ();
sub g { { FIRST @log.push('in-sub') } }
g() for ^2;
is-deeply @log, ['in-sub', 'in-sub'], 'a bare block in a sub, per call';

@log = ();
for ^2 { given $_ { when 1 { FIRST @log.push('when') }; default { FIRST @log.push('default') } } }
is-deeply @log, ['default', 'when'], 'when and default bodies';

@log = ();
for ^2 { try { FIRST @log.push('try'); 1 } }
is-deeply @log, ['try', 'try'], 'a try block';

@log = ();
my $c = { { FIRST @log.push('nested') }; FIRST @log.push('outer') };
$c(); $c();
is-deeply @log, ['outer', 'nested', 'nested'],
    'a closure fires its own FIRST once, its inner block on every call';

@log = ();
for ^3 { FIRST @log.push('loop') }
is-deeply @log, ['loop'], 'a loop body still fires once';
