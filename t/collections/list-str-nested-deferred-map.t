use Test;

# A `.map`/`gather` Seq nested inside a list has not run its callback when
# the outer list is stringified. `.Str`, `.join` and interpolation must run
# it, not render its empty seed (Pod::To::HTML's table rows came out as
# `<tr>  </tr>`).

plan 9;

{
    my $l = ((3, 4).map({ $_ }),);
    is $l.Str, '3 4', '.Str of a list holding an un-run map Seq';
}
{
    my $k = (3, 4).map({ $_ });
    my $l = ($k,);
    is "$l", '3 4', 'interpolating a list holding an un-run map Seq';
}
{
    my $k = (3, 4).map({ $_ });
    is (($k,),).join('|'), '3 4', '.join over a list nesting an un-run map Seq';
}
{
    my $k = (3, 4).map({ $_ });
    is ['t', ($k,).Seq].join('|'), 't|3 4', '.join of an Array with a Seq of a map Seq';
}
{
    my $s = (1, 2).map(-> $x { 'a', (3, 4).map({ $_ * $x }), 'b' });
    my @r;
    @r.push('t', $s);
    is @r.join('|'), 't|a 3 4 b a 6 8 b', 'map returning lists with inner maps, pushed then joined';
}
{
    my $s = (1,).map(-> $x { (3, 4).map({ $_ * $x }) });
    my @y = 't', $s;
    is @y.join('|'), 't|3 4', 'map of maps stored in an array, joined';
}
{
    my $g = gather { take 5; take 6 };
    my $l = ($g,);
    is $l.Str, '5 6', '.Str of a list holding an un-run gather';
}
{
    my $k = (1, 2, 3).grep(* > 1);
    is (($k,),).Str, '2 3', '.Str of a list nesting an un-run grep';
}
{
    # The table renderer shape from Pod::To::HTML.
    my @rows = [<a b>], [<c d>];
    my @r = '<tbody>', @rows.map(-> $line {
        '<tr>', $line.list.map(-> $cell { "<td>{ $cell }</td>" }), '</tr>'
    }), '</tbody>';
    is @r.join("\n"),
       "<tbody>\n<tr> <td>a</td> <td>b</td> </tr> <tr> <td>c</td> <td>d</td> </tr>\n</tbody>",
       'nested map rows render their cells';
}
