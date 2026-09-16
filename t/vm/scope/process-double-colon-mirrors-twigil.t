use Test;

plan 3;

# `$PROCESS::OUT = ...` is the double-colon-qualified spelling of the
# process-level dynamic variable that `$*OUT` and the sigilless `*OUT` also
# name (BASE_TIER_DYNAMICS seeds all three spellings together at startup).
# The write path mapped `$PROCESS::OUT` only to the sigilless `*OUT` key and
# left `$*OUT` stale, so a `print`/`say` after redirecting `$PROCESS::OUT`
# kept writing to the ORIGINAL handle instead of the reassigned one --
# discovered via Acme::Anguish's dependency Test::Output, whose `capture`
# idiom reassigns `$PROCESS::OUT`/`$PROCESS::ERR` to a custom IO::Handle
# subclass around the code under test.
my class Capture is IO::Handle {
    has @.contents;

    submethod TWEAK {
        self.encoding: 'utf8';
    }

    method WRITE( IO::Handle:D: Blob:D \data --> Bool:D ) {
        @.contents.push: data.decode();
        True;
    }
}

my $orig-out = $PROCESS::OUT;
my $out = Capture.new;
$PROCESS::OUT = $out;
print "captured";
$PROCESS::OUT = $orig-out;

is $out.contents.join(''), 'captured',
    '$PROCESS::OUT = ... redirects print through the new handle';

is $*OUT, $orig-out, '$*OUT reads back the restored handle afterwards';

# The read side already resolved through PROCESS::, so pin that a plain
# `$*OUT` scalar read also observes a `$PROCESS::OUT` write immediately.
my $x = 99;
$PROCESS::OUT = $x;
my $seen = $*OUT;
$PROCESS::OUT = $orig-out;
is $seen, $x, '$*OUT reads the value $PROCESS::OUT was just set to';
