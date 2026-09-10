use Test;

plan 4;

# `.created` / `.modified` / `.accessed` / `.changed` are `Instant`s, and raku
# reports them to sub-second resolution. Truncating to whole seconds made two
# writes inside the same second compare equal, so any "is the file on disk newer
# than my cached copy?" check never fired (Template::Nest::Fast's
# `:advanced-indexing` re-index).

my $dir  = $*TMPDIR.add("mutsu-mtime-{$*PID}-{(^100000).pick}");
$dir.mkdir;
my $file = $dir.add('stamp.txt');

$file.spurt('one');
my $first = $file.modified;

sleep 0.05;

$file.spurt('two-and-a-bit-longer');
my $second = $file.modified;

isa-ok $first, Instant, '.modified returns an Instant';
ok $second > $first, 'a rewrite inside the same second is strictly newer';
ok $first.Rat - $first.Rat.Int != 0 || $second.Rat - $second.Rat.Int != 0,
   'at least one timestamp carries a sub-second part';
ok $file.changed >= $first, '.changed is at least as new as the first write';

$file.unlink;
$dir.rmdir;
