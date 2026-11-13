use lib 't/lib';
use Test;

# A unit `Perl6::Compiler.eval` runs is a compilation unit of its own,
# compiled inside the session's previous unit and nothing else (#17087).
# rakudo gives a session started without an outer context a fresh GLOBAL, so a
# class the module running the session declared is not visible from it, and a
# session `sub` of the same spelling is what the name means; a class the
# session declares is GLOBAL's. Identical under rakudo.

plan 5;

use ReplSandboxOut;

my $sb = ReplSandbox.new;
is $sb.run('Out[1]'), '2', 'a session sub wins over a class the running module declared';
is $sb.run('Out.elems'), '2', 'the bare name calls the session sub';
is $sb.run('$Out.elems'), '2', 'the session scalar of that spelling is still there';
# rakudo spells it `GLOBAL::ReplZork`: the session's GLOBAL is a fresh one.
like $sb.run('class ReplZork { }; ReplZork.^name'), /^ ['GLOBAL::']? 'ReplZork' $/,
    'a class a session unit declares is not named after the compiler class';
like $sb.run('ReplZork.^name'), /^ ['GLOBAL::']? 'ReplZork' $/, 'and a later unit sees it';
