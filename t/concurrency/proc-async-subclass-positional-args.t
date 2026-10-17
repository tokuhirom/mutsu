use Test;

class P is Proc::Async { has $.label = 'custom'; }

my $p = P.new($*EXECUTABLE, '-e', 'say 42');
is-deeply $p.command, ($*EXECUTABLE, '-e', 'say 42'),
    'the inherited Proc::Async constructor preserves positional command arguments';
is $p.label, 'custom', 'the subclass default attributes are initialized';
my $out = '';
$p.stdout.tap(-> $chunk { $out ~= $chunk });
await $p.start;
is $out.trim, '42', 'a subclass instance uses Proc::Async output rows';

done-testing;
