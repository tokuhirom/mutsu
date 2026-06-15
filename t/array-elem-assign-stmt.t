# A statement-level array element/slice assignment with no space before the `=`
# (`@a[0]=1`) was mis-parsed as a reduction compound assignment (`@a [0]= 1`),
# throwing "Unsupported reduction operator: 0". The `[op]=` reduction metaop must
# not swallow a postfix `[index]` subscript on the lvalue. (It only reproduced at
# statement level; in expression context the normal parser was unaffected.)
use Test;

plan 14;

{
    my @a = 0, 0, 0;
    @a[0]=1;
    is @a.join(','), '1,0,0', 'tight @a[0]=1 at statement level';
}
{
    my @a = 0, 0, 0;
    @a[2]=9; @a[0]=1;
    is @a.join(','), '1,0,9', 'two tight element assigns';
}
{
    my @a = 0, 0, 0;
    my $i = 1;
    @a[$i]=7;
    is @a.join(','), '0,7,0', 'tight @a[$i]=7 (variable index)';
}
{
    my @a = 1, 2, 3;
    @a[*-1]=0;
    is @a.join(','), '1,2,0', 'tight @a[*-1]=0 (Whatever end index)';
}
{
    my @a = 1, 2, 3, 4;
    @a[1,2]=8,9;
    is @a.join(','), '1,8,9,4', 'tight @a[1,2]=8,9 (slice)';
}
{
    my @a = 0, 0;
    @a[0]+=5;
    is @a.join(','), '5,0', 'tight @a[0]+=5 (compound)';
}
{
    my %h;
    %h{"k"}=3;
    is %h<k>, 3, 'tight %h{"k"}=3 (hash subscript)';
}
{
    my @a = 0, 0;
    for 0..0 { @a[0]=1 }
    is @a.join(','), '1,0', 'tight element assign inside a block';
}
{
    my @a = 0, 0;
    sub setit(@x) { @x[1]=9 }
    setit(@a);
    is @a.join(','), '0,9', 'tight element assign inside a sub';
}

# The `[op]=` reduction compound assignment still works (space before `[`).
{
    my $s = 5;
    $s [+]= 3;
    is $s, 8, 'reduction compound [+]= still works';
}
{
    my @a = 1, 2, 3;
    @a [*]= 2;
    is @a, 6, 'reduction compound [*]= still works';
}
{
    my @a = 1, 2, 3;
    @a [**]= 2;
    is @a, 9, 'reduction compound [**]= still works';
}
{
    my @a;
    my @b = 1, 2;
    @a [R,]= @b;
    is @a.elems, 2, 'reverse-comma reduction [R,]= still works';
}
{
    my @a = 1, 2, 3;
    @a [max]= 5;
    is @a, 5, 'word reduction [max]= still works';
}
