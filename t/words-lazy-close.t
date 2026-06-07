use Test;

# Lazy `words($fh)` / `$fh.words` with close-on-exhaust semantics, plus
# IO::ArgFiles.new(@files). See roast/S16-io/words.t for the full spec.

plan 11;

my $file = $*TMPDIR.add("mutsu-words-lazy-{$*PID}.txt");
$file.spurt("alpha beta gamma\ndelta epsilon\n");
LEAVE { $file.unlink }

# --- &words sub form ---------------------------------------------------------

# Full consumption yields all words in order.
{
    my $fh = $file.open;
    is-deeply words($fh).List, <alpha beta gamma delta epsilon>,
        '&words drains all words';
    $fh.close;
}

# `:close` with full consumption closes the handle.
{
    my $fh = $file.open;
    my @res = words($fh, :close);
    is-deeply @res.List, <alpha beta gamma delta epsilon>, ':close full read words';
    is $fh.opened, False, ':close full read closes handle';
}

# `:close` with a partial slice leaves the handle OPEN (close-on-exhaust only).
{
    my $fh = $file.open;
    my @res = words($fh, :close)[1, 2];
    is-deeply @res.List, <beta gamma>, ':close partial slice words';
    is $fh.opened, True, ':close partial slice keeps handle open';
    $fh.close;
}

# A `$limit` reads exactly that many words.
{
    my $fh = $file.open;
    is-deeply words($fh, 2).List, <alpha beta>, '$limit words';
    $fh.close;
}

# --- IO::Handle.words method form -------------------------------------------

{
    my $fh = $file.open;
    is-deeply $fh.words.List, <alpha beta gamma delta epsilon>, '.words method drains all';
    $fh.close;
}

# Numeric coercion of the lazy iterator counts elements.
{
    my $fh = $file.open;
    is +$fh.words, 5, '+$fh.words counts words';
    $fh.close;
}

# --- IO::ArgFiles.new(@files) ------------------------------------------------

my $f1 = $*TMPDIR.add("mutsu-argf1-{$*PID}.txt");
my $f2 = $*TMPDIR.add("mutsu-argf2-{$*PID}.txt");
$f1.spurt("foo bar");
$f2.spurt("baz qux");
LEAVE { $f1.unlink; $f2.unlink }

{
    my $af = IO::ArgFiles.new($f1, $f2);
    isa-ok $af, IO::Handle, 'IO::ArgFiles.new returns a handle';
    is-deeply $af.words.List, <foo bar baz qux>,
        'IO::ArgFiles.new reads words across all files';
}

# words() with no args reads from $*ARGFILES.
{
    my $*ARGFILES = IO::ArgFiles.new($f1, $f2);
    is-deeply words().List, <foo bar baz qux>, 'words() uses $*ARGFILES';
}
