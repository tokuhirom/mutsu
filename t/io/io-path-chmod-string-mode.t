use Test;
plan 6;

# `chmod` coerces its mode with Int(), as rakudo's signature does: the string
# '0444' is the decimal 444 (mode 0o674 -- the documented trap), '0o444' is an
# octal literal in a string.
my $file = $*TMPDIR.add("mutsu-chmod-string-mode-$*PID");
$file.spurt('x');
LEAVE $file.unlink;

sub mode-of(IO::Path $p) { $p.mode.Str }

ok $file.chmod('0444'), "chmod with the string '0444' succeeds";
is mode-of($file), '0674', "'0444' is decimal 444, i.e. mode 0674";
ok $file.chmod('0o444'), "chmod with the string '0o444' succeeds";
is mode-of($file), '0444', "'0o444' is octal 0444";
ok $file.chmod(0o600), 'chmod with an octal literal succeeds';
is mode-of($file), '0600', 'an octal literal sets that mode';
