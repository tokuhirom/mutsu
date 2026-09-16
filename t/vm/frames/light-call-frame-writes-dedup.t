use Test;

plan 2;

# `Env::frame_writes` (ADR-0004 J4d, `Env::flattened_for_frame`) logs the
# by-name writes a light-called frame makes, so its return can merge just
# those names back into the caller instead of rescanning the whole scope.
# The log is documented as a *set* of names to consider ("order does not
# matter" -- `Env::retain_frame_writes`), but it used to be appended to
# unconditionally, with no dedup.
#
# The common lazy-singleton-accessor idiom --
#
#     submethod instance { $instance = T.bless unless $instance; $instance.make() }
#
# -- calls `.make()` one level of native-call nesting deeper than calling it
# directly (`T.bless.make()`) from a flat top-level scope. That extra nesting
# was enough to arm `.make()`'s own frame's write log for its WHOLE
# execution. Every block mutsu calls via `call_sub_value`
# (`.map`/`.grep`/`.classify`/...) inside `.make()` is its own nested light
# call, and each one's return-time writeback merge re-inserts its surviving
# keys into that SAME armed log. Processing data through several such
# pipeline stages, each rewriting the same handful of class-scoped `my`
# variables, appended one entry per call to an ever-growing
# `Arc<Vec<Symbol>>` with no dedup -- and the moment that Arc was shared with
# a saved caller env (`push_call_frame`, once per nested call), the next
# append had to `Arc::make_mut`-deep-copy the whole (already huge) log. That
# is O(n) extra work per call, O(n^2) over `.make()`'s lifetime.
#
# This is a reduced form of the real-world trigger (#8489): a
# `Data::Generators::ResourceAccess`-shaped class whose `.make()` parses two
# CSVs through `slurp`/`split`/`grep`/a `do for` loop/`classify`/`Bag`, on an
# 85_000-row real file going from ~21s (called directly) to not finishing in
# 60s+ (called through the `submethod instance` singleton accessor). Calling
# `.make()` directly and calling it through the singleton should cost about
# the same -- the regression this pins is specifically the "via the
# singleton" path taking multiples of the direct one, not the pipeline's own
# baseline cost (which may itself be somewhat superlinear on both paths).
#
# The file paths are hardcoded string literals read by `slurp`, exactly as
# the real module's `.make()` reads `%?RESOURCES<...>.open` -- routing the
# path through a signature parameter or a dynamic variable instead changed
# this test's own env shape enough to mask the regression it is meant to
# pin, so it is deliberately avoided here.

my $words-csv-path = "tmp/light-call-frame-writes-dedup-words-$*PID.csv";
my $pets-csv-path = "tmp/light-call-frame-writes-dedup-pets-$*PID.csv";
my $words-csv = $words-csv-path.IO;
my $pets-csv = $pets-csv-path.IO;
LEAVE { .unlink for $words-csv, $pets-csv }

sub gen-words-csv(IO::Path $path, Int $size) {
    my $rows = (1..$size).map({ $_ ~ ',True,' ~ ($_ %% 2 ?? 'True' !! 'False') ~ ',False' }).join("\n");
    $path.spurt("Word,KnownWordQ,CommonWordQ,StopWordQ\n" ~ $rows);
}

sub gen-pets-csv(IO::Path $path, Int $size) {
    my @species = <Dog Cat Bird>;
    my $rows = (1..$size).map({
        '"' ~ @species[$_ % 3] ~ '","Name' ~ $_ ~ '",' ~ ($_ % 500)
    }).join("\n");
    $path.spurt("Species,Name,Count\n" ~ $rows);
}

class ResourceAccess {
    my @englishWords;
    my %englishWords;
    my %typeToIndexes;
    my %specieToPetNames;
    my $petNameToCount;

    my ResourceAccess $instance = Nil;
    my Int $numberOfInstances = 0;
    my Int $numberOfMakeCalls = 0;

    method new {!!!}

    method reset() {
        $instance = Nil;
        $numberOfInstances = 0;
    }

    submethod instance {
        $instance = ResourceAccess.bless unless $instance;
        if $numberOfInstances == 0 {
            $instance.make()
        }
        $numberOfInstances += 1;
        $instance
    }

    method make() {
        $numberOfMakeCalls += 1;

        my $fileName = "tmp/light-call-frame-writes-dedup-words-$*PID.csv";
        my $text = slurp $fileName;
        @englishWords = $text.split("\n").map({ $_.split(',') });
        @englishWords = @englishWords[1..*-1];

        my $k = 0;
        @englishWords = do for @englishWords -> $row {
            ($row[0], $row[1] eq 'True', $row[2] eq 'True', $row[3] eq 'True', $k++)
        }

        %englishWords = @englishWords.map({ $_[0] => $_ });

        %typeToIndexes =
            known => @englishWords.grep({ $_[1] }).map({ $_[4] }),
            common => @englishWords.grep({ $_[2] }).map({ $_[4] }),
            stopword => @englishWords.grep({ $_[3] }).map({ $_[4] });

        $fileName = "tmp/light-call-frame-writes-dedup-pets-$*PID.csv";
        $text = slurp $fileName;
        my @petNames = $text.split("\n").map({ $_.split('",').List });
        @petNames = @petNames[1..*-1];
        @petNames = @petNames.grep({ $_.elems == 3 });

        @petNames = do for @petNames -> $row {
            ($row[0].substr(1, *), $row[1].substr(1, *), +$row[2])
        }

        %specieToPetNames = @petNames.classify({ $_[0] }).map({
            $_.key.lc => Bag($_.value.map({ $_[1] => $_[2] }))
        });

        $petNameToCount = Bag([(+)] %specieToPetNames.values);

        self
    }
}

'tmp'.IO.mkdir;
gen-words-csv($words-csv, 5000);
gen-pets-csv($pets-csv, 2000);

my $t0 = now;
ResourceAccess.bless.make;
my $direct = (now - $t0).Num;

ResourceAccess.reset;
$t0 = now;
ResourceAccess.instance;
my $via = (now - $t0).Num;

ok $direct > 0, 'the direct path took measurable time';
# Fixed, the two paths cost about the same (measured ~1.0x, run to run).
# Pre-fix, the "via" path measured ~2x the "direct" one at this size (and
# diverges further as size grows, since the underlying cost is O(n^2) over
# `.make()`'s own lifetime). 1.5x sits between the two with margin on both
# sides, so this catches the regression without flagging ordinary noise.
ok $via < $direct * 1.5,
    "calling .make() through the singleton doesn't cost multiples of calling "
    ~ "it directly (direct $direct.fmt('%.3f')s, via $via.fmt('%.3f')s)";
