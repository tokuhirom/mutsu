use Test;

# Distribution: CSS::Stylesheet (`my $actions .= $module.actions.new`).
plan 4;

my $up = sub ($x) { "hello" };
my $o .= $up.uc;
is $o, "hello", 'my $x .= $callable assigns the indirect call; the chain is sunk';

my $bang = sub ($x) { $x.defined ?? "def" !! "undef" };
my $p .= $bang;
is $p, "undef", 'my $x .= $callable calls it with the new variable as invocant';

my $q = "abc";
my $add = sub ($x) { $x ~ "!" };
$q .= $add;
is $q, "abc!", 'non-declaration form still works';

my $count = 0;
my $inc = sub ($x) { $count++; 5 };
my $s .= $inc.Str;
is $count, 1, 'the chained call after the indirect call runs exactly once for the call';
