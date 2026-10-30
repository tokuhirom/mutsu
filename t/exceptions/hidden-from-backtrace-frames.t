use Test;

# A routine declared `is hidden-from-backtrace` is no line of a backtrace's
# text -- a named sub, and an anonymous `sub` as well (static's
# warn-on-* diagnostics are raised from `map(sub ... is hidden-from-backtrace)`).

plan 5;

sub hidden-named($x) is hidden-from-backtrace { die "named" }
sub caller-of-named { hidden-named(1) }
try caller-of-named();
my $text = $!.backtrace.Str;
unlike $text, /hidden\-named/, 'a hidden named sub is not in the text';
like $text, /'sub caller-of-named'/, 'its caller is';

my $anon = sub ($d) is hidden-from-backtrace { die "anon" };
sub caller-of-anon { $anon(1) }
try caller-of-anon();
$text = $!.backtrace.Str;
like $text, /'sub caller-of-anon'/, 'the caller of a hidden anonymous sub is in the text';
is $text.lines.grep(/'in block <unit>'/).elems, 1, 'the hidden anonymous sub contributes no line of its own';

my $err = run($*EXECUTABLE, '-e',
    'sub u { (1,).map(sub ($d) is hidden-from-backtrace { warn "W" }).sink }; u()', :err).err.slurp;
is $err.lines.join('|'), 'W|  in sub u at -e line 1', 'warn shows the visible caller only';
