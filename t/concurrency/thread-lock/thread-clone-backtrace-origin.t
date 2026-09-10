use Test;

# A pending `.then` callback runs in a thread clone. Its anonymous callback
# frame is hidden from the concise backtrace, so the clone must retain the
# source location of the spawn as the enclosing visible frame.
my $source = Promise.new;
my $derived = $source.then(-> $antecedent { $antecedent.result });
$source.break('broken source');
try $derived.result;

my $backtrace = $derived.cause.backtrace;
ok $backtrace.Str.chars > 0,
    'a thread-clone callback cause has a rendered backtrace';
like $backtrace.Str, / 'at ' .* 'line' /,
    'the rendered backtrace carries a source location';
ok $backtrace.list.grep({ .file.chars && .line > 0 }).elems > 0,
    'the structured backtrace carries a file and line';

# A Promise.start entry block already has its own location. Keep the existing
# regression guarantee that a spawn origin does not duplicate that frame.
my $started = Promise.start({ die 'broken start' });
try $started.result;
my @lines = $started.cause.backtrace.Str.lines.grep(*.trim.chars);
ok @lines.elems >= 1, 'a Promise.start failure still has a backtrace frame';
is @lines.unique.elems, @lines.elems,
    'a spawn origin does not duplicate a located worker entry frame';

done-testing;
