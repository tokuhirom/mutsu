use Test;

# An interpolation whose *subscript* ends exactly where a repeated delimiter's
# closing run begins: `qq[[@a[0]]]`.
#
# mutsu used to find a quote's close with a purely textual scan and only then
# hand the extracted text to the interpolator. With a repeated delimiter that
# split got this wrong: the text after `[[` is `@a[0]]]`, whose first `]]` sits
# immediately after the `0`, so the scan stopped there and yielded the
# unterminated `@a[0`. The close scan now steps over a whole interpolation atom,
# measured by the very function that will later consume it.
#
# This is NOT single-bracket nesting -- `qq[[a[b]]]` stays a syntax error in
# rakudo and here, because with no sigil nothing consumes the `[b]`. That case
# and the neighbouring correct ones are pinned by `t/quote-doubled-delimiter.t`;
# this file pins the interpolating half. Measured against rakudo 2026.07.

plan 19;

my @a = 1, 2, 3;
my %h = a => 5, b => 6;
my $x = 'X';
my $s = 'abc';

# --- the headline: a subscript abutting the closing run ----------------------
is qq[[@a[0]]], '1', 'an indexed array abutting the `]]` close';
is qq[[@a[1]@a[2]]], '23', 'two of them, the second abutting the close';
is qq[[%h<a>]], '5', 'an angle hash subscript abutting the close';
is qq[[%h<a>%h<b>]], '56', 'two of those';
is qq[[%h{'b'}]], '6', 'a braced hash subscript';
is qq[[@a[*-1]]], '3', 'a Whatever-relative index';
is qq<<@a[0]>>, '1', 'the same shape with a doubled angle delimiter';
is qq[[@a[0,1]]], '1 2', 'a slice subscript abutting the close';
is qq[[%h<<a>>]], '5', 'a doubled-angle hash subscript abutting the close';
is qq{{@a[0]}}, '1', '`{{ }}`, where `}` is not the subscript bracket';

# --- the neighbours that already worked, so they cannot regress --------------
is qq[@a[0]], '1', 'a single delimiter, via incidental bracket nesting';
is qq[[x@a[0]y]], 'x1y', 'a subscript that does not abut the close';
is qq[[$x]], 'X', 'a plain scalar';
is qq[[{ 1 + 1 }]], '2', 'a closure interpolation';
is qq[[@a[0] and @a[1]]], '1 and 2', 'a subscript followed by literal text';
is qq[[$s.uc()]], 'ABC', 'a method call on an interpolated scalar';

# --- the scan must not start consuming un-sigiled brackets -------------------
throws-like { EVAL 'qq[[a[b]]]' }, Exception,
    'a bareword bracket still does not balance a doubled delimiter';
throws-like { EVAL 'qq[[a]]]' }, Exception,
    'a trailing stray bracket is still a syntax error';
is qq[[a] [b]], 'a] [b', 'an unbalanced bracket with no sigil is plain content';
