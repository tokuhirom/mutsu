use Test;

# `Str.ACCEPTS` compares the matched text, so a Match (or a capture)
# smartmatches the string it captured. zef's SystemQuery dispatches on
# `given $/[0] { when 'distro' { ... } }`, which fell through every `when`.

plan 5;

"by-distro.name" ~~ /^'by-' (distro|kernel)/;
ok $/[0] ~~ 'distro', 'a capture smartmatches its text';
nok $/[0] ~~ 'kernel', 'and not some other string';
ok 'distro'.ACCEPTS($/[0]), 'Str.ACCEPTS takes the capture';
ok $/ ~~ 'by-distro', 'the whole match smartmatches its text';

my $hit = do given $/[0] {
    when 'kernel' { 'k' }
    when 'distro' { 'd' }
};
is $hit, 'd', 'given/when dispatches on the captured text';
