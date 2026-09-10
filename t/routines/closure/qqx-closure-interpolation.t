use Test;

plan 6;

# qqx (and backtick command quoting) must do full qq interpolation, including
# `{ EXPR }` closure blocks — not just `$var`. (PDF::Extract's `.text` builds its
# pdftotext command line with `qqx\`... {$!first} ... '{$.file}' ...\``.)

my $x = 5;

is qqx/echo {$x + 1}/.chomp, '6', 'qqx: closure block interpolates';
is qqx/echo $x/.chomp, '5', 'qqx: scalar interpolation still works';
is (qqx`echo {$x * 2}`).chomp, '10', 'backtick: closure block interpolates';

# Attribute accessors inside a `{ }` block (the PDF::Extract shape).
class C {
    has $.file = "myfile.txt";
    has $.first is rw = 1;
    has $.last  is rw = 0;
    method cmd {
        (qqx`echo -f {$!first} -l {$!last} '{$.file}' -`).chomp
    }
}
is C.new.cmd, "-f 1 -l 0 myfile.txt -", 'qqx interpolates attribute accessors in { } blocks';

# Bracketed delimiters interpolate closures; a brace delimiter keeps inner
# braces literal (they are the quote delimiters).
is qqx[echo {$x + 1}].chomp, '6', 'qqx[...] interpolates closures';
is qqx{echo hi}.chomp, 'hi', 'qqx{...} runs (brace delimiter)';
