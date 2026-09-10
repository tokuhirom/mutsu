use Test;

# A `|(...)` Slip passed to a list operator (say/put/print/note) spreads its
# elements as individual arguments, like the parenthesized-call form.
# `put` with multiple arguments concatenates their .Str and adds one newline.

plan 17;

# --- Slip flattening into say/print/put ---
is run-say('say |(1,2,3)'), "123\n", 'say flattens a leading Slip arg';
is run-say('say 1, |(2,3), 4'), "1234\n", 'say flattens a mid-list Slip arg';
is run-say('say |(1,2,3), |(4,5)'), "12345\n", 'say flattens multiple Slip args';
is run-say('my @a = <a b c>; say |@a'), "abc\n", 'say flattens a Slip from an array';
is run-say('print |(1,2,3)'), "123", 'print flattens a Slip arg';

# --- A parenthesized list is NOT a Slip: stays one list argument ---
is run-say('say (1,2,3)'), "(1 2 3)\n", 'a bare list arg is a single gisted value';

# --- put: multiple args concatenate into one line ---
is run-say('put 1,2,3'), "123\n", 'put concatenates multiple args on one line';
is run-say('put "a","b"'), "ab\n", 'put concatenates string args';
is run-say('put |(1,2,3)'), "123\n", 'put flattens a Slip then concatenates';
is run-say('my @a=1,2,3; put |@a'), "123\n", 'put flattens an array Slip';
is run-say('put 42'), "42\n", 'put of a single value';
is run-say('my @a=1,2,3; put @a'), "1 2 3\n", 'put of an array stringifies with spaces';

# --- put threads a lone Junction onto separate lines ---
is run-say('put 1|2'), "1\n2\n", 'put autothreads a lone Junction arg';

# --- say does NOT thread a Junction; it gists it ---
is run-say('say 1|2'), "any(1, 2)\n", 'say gists a Junction without threading';

# --- note flattens Slips to STDERR ---
is run-note('note |(1,2,3)'), "123\n", 'note flattens a Slip arg';

# --- empty Slip contributes nothing ---
is run-say('say 1, |(), 2'), "12\n", 'an empty Slip arg contributes nothing';
is run-say('put |()'), "\n", 'put of only an empty Slip is just a newline';

sub run-say($code) {
    my $proc = run($*EXECUTABLE, '-e', $code, :out);
    $proc.out.slurp(:close);
}
sub run-note($code) {
    my $proc = run($*EXECUTABLE, '-e', $code, :err);
    $proc.err.slurp(:close);
}
