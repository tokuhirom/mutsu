use Test;

# Slice 2b (docs/scalar-array-sharing.md): `@aoa[i] = @row` / `%h<k> = @row`
# stores the SAME array object in the element (raku reference semantics), so
# structural mutations through either name are visible to both. A later
# non-share reassignment of the element REPLACES it (value semantics).

plan 28;

# --- array element forward share ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @row.push(9);
    is-deeply @aoa[0], [1, 2, 9], 'source push visible via element (forward)';
}

# --- array element reverse share ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @aoa[0].push(8);
    is-deeply @row, [1, 2, 8], 'element push visible via source (reverse)';
}

# --- element write through source ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @row[0] = 99;
    is-deeply @aoa[0], [99, 2], 'source element write visible via element';
}

# --- element write through element ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @aoa[0][1] = 77;
    is-deeply @row, [1, 77], 'element element-write visible via source';
}

# --- source whole-reassign (list assign mutates same object) ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @row = (9, 9);
    is-deeply @aoa[0], [9, 9], 'source whole list-reassign visible via element';
}

# --- element reassign to another array => replace, source untouched ---
{
    my @row = (1, 2);
    my @other = (7, 8);
    my @aoa;
    @aoa[0] = @row;
    @aoa[0] = @other;
    @row.push(9);
    is-deeply @aoa[0], [7, 8], 'element reassigned to another array replaces';
    is-deeply @row, [1, 2, 9], 'original source untouched after element reassign';
}

# --- element reassign to scalar => replace, source untouched ---
{
    my @row = (1, 2);
    my @aoa;
    @aoa[0] = @row;
    @aoa[0] = 42;
    @row.push(9);
    is @aoa[0], 42, 'element reassigned to scalar replaces (no write-through)';
    is-deeply @row, [1, 2, 9], 'source untouched after scalar reassign of element';
}

# --- re-share same element to a different source ---
{
    my @a = (1, 2);
    my @b = (5, 6);
    my @c;
    @c[0] = @a;
    @c[0] = @b;
    @a.push(7);
    @b.push(8);
    is-deeply @c[0], [5, 6, 8], 're-shared element tracks new source';
    is-deeply @a, [1, 2, 7], 'first source detached after re-share';
}

# --- multiple elements share independent sources ---
{
    my @x = (1, 2);
    my @y = (3, 4);
    my @aoa;
    @aoa[0] = @x;
    @aoa[1] = @y;
    @x.push(99);
    @y.push(88);
    is-deeply @aoa[0], [1, 2, 99], 'first element tracks its own source';
    is-deeply @aoa[1], [3, 4, 88], 'second element tracks its own source';
}

# --- AoA build loop ---
{
    my @rows;
    for 1..3 -> $i {
        my @r = ($i, $i * 10);
        @rows[$i - 1] = @r;
        @r.push(999);
    }
    is-deeply @rows, [[1, 10, 999], [2, 20, 999], [3, 30, 999]], 'AoA build loop shares each row';
}

# --- nested deref read ---
{
    my @e = (10, 20);
    my @f;
    @f[0] = @e;
    @e.push(30);
    is @f[0][2], 30, 'nested index read sees shared push';
}

# --- hash value forward share ---
{
    my @row = (1, 2);
    my %h;
    %h<k> = @row;
    @row.push(9);
    is-deeply %h<k>, [1, 2, 9], 'hash value: source push visible';
}

# --- hash value reverse share ---
{
    my @row = (1, 2);
    my %h;
    %h<k> = @row;
    %h<k>.push(8);
    is-deeply @row, [1, 2, 8], 'hash value: element push visible via source';
}

# --- hash value reassign to scalar => replace ---
{
    my @row = (1, 2);
    my %h;
    %h<k> = @row;
    %h<k> = 42;
    @row.push(9);
    is %h<k>, 42, 'hash value reassigned to scalar replaces';
    is-deeply @row, [1, 2, 9], 'source untouched after hash value scalar reassign';
}

# --- hash value reassign to another array => replace ---
{
    my @row = (1, 2);
    my @other = (7, 8);
    my %h;
    %h<k> = @row;
    %h<k> = @other;
    @row.push(9);
    is-deeply %h<k>, [7, 8], 'hash value reassigned to another array replaces';
    is-deeply @row, [1, 2, 9], 'source untouched after hash value array reassign';
}

# --- multiple hash keys ---
{
    my @p = (1, 2);
    my @q = (3, 4);
    my %h;
    %h<a> = @p;
    %h<b> = @q;
    @p.push(9);
    is-deeply %h<a>, [1, 2, 9], 'hash key a tracks source';
    is-deeply %h<b>, [3, 4], 'hash key b unaffected';
}

# --- plain @-target copy still copies (NOT shared) ---
{
    my @row = (1, 2);
    my @copy = @row;
    @row.push(9);
    is-deeply @copy, [1, 2], 'plain @copy = @row stays a copy';
}

# --- hash whole-value reverse element write ---
{
    my @row = (1, 2);
    my %h;
    %h<k> = @row;
    %h<k>[0] = 50;
    is-deeply @row, [50, 2], 'hash value element-write visible via source';
}

# --- self-reference: `%h<k> = %h` keeps the infinite-HoH path (no share) ---
{
    my %hash = (val => 42);
    %hash<ref> = %hash;
    isa-ok %hash, Hash, 'self-ref: %hash still isa Hash';
    isa-ok %hash<ref>, Hash, 'self-ref: %hash<ref> isa Hash';
    is %hash<ref><val>, 42, 'self-ref: infinite HoH access works';
}
