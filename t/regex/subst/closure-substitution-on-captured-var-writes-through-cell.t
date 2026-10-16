use Test;

# `$x ~~ s///` / `$x ~~ tr///` inside a closure another routine runs writes
# the captured `$x`: the closure counts the smartmatch's write-back as a write
# of `$x`, so `$x` is a shared cell, and the write-back stores into that cell.
# The declaring block and every other closure see the result without a frame's
# return merge carrying it (ADR-12529 §3, slice 33). Checked against rakudo.

plan 6;

sub run(&c) { c() }

{
    my $s = 'aba';
    my &peek = { $s };
    run { $s ~~ s/b/X/ };
    is $s, 'aXa', 'a substitution in a closure reaches the block';
    is peek(), 'aXa', 'and a closure made before it';
}

{
    my $g = 'aaa';
    run { $g ~~ s:g/a/b/ };
    is $g, 'bbb', 'a global substitution';
}

{
    my $t = 'abc';
    run { $t ~~ tr/a..c/A..C/ };
    is $t, 'ABC', 'a transliteration';
}

{
    my $u = 'x1';
    run { $u ~~ s/\d/{ $u.chars }/ };
    is $u, 'x2', 'a replacement that reads the variable';
}

{
    my $r = 'aba';
    run { $r ~~ / b / };
    is $r, 'aba', 'a plain match leaves the variable alone';
}
