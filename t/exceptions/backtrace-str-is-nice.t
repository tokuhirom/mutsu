use Test;

# Backtrace.Str is .nice: the interesting frames, newline-terminated, for every
# way of stringifying it (Type/Exception.rakudoc).
plan 6;

sub bt { try die "x"; $!.backtrace }
my $b = bt();

is $b.Str, $b.nice, '.Str is .nice';
ok $b.Str.ends-with("\n"), '.Str ends in a newline';
is "$b", $b.nice, 'string interpolation is .nice';
is ~$b, $b.nice, 'prefix ~ is .nice';
is $b.Str.lines.elems, $b.nice.lines.elems, 'one line per interesting frame';
nok $b.Str.contains('throw'), 'the internal throw frame is not shown';
