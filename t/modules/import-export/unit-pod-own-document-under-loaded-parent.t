use Test;

# From the Sway::Config ecosystem distribution: `unit module Outer::Inner`
# that `use`s its parent package read the parent's `$=pod` in its mainline
# instead of its own document.

plan 3;

my $dir = $*TMPDIR.add("mutsu-unitpod-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.add('Outer').dir; $dir.add('Outer').rmdir; .unlink for $dir.dir; $dir.rmdir } }
$dir.add('Outer.rakumod').spurt: "unit module Outer;\n\n=NAME outer-doc\n\nour sub n \{ \$=pod.elems }\n";
$dir.add('Outer').mkdir;
$dir.add('Outer/Inner.rakumod').spurt: q:to/END/;
    use Outer;
    unit module Outer::Inner;

    =NAME inner-doc

    =TITLE inner title

    our $count = $=pod.elems;
    our $first = $=pod[0].contents[0].contents[0];
    END

my $proc = run $*EXECUTABLE, '-I', $dir.absolute, '-e',
    'use Outer; require Outer::Inner; say $Outer::Inner::count; say $Outer::Inner::first; say Outer::n()',
    :out, :err;
my @out = $proc.out.lines;
is @out[0], '2', 'the inner unit sees its own two Pod blocks';
is @out[1], 'inner-doc', 'and its own first block';
is @out[2], '1', 'the parent still sees its own document';
