use Test;

# Two independent S09-subscript/slice.t features:
#  1. HyperWhatever (`**`) hammer index: `@a[**]` recursively descends nested
#     arrays and returns every leaf as a flat list (test 56).
#  2. Nested lazy slice BIND (`:=`) in expression context: unlike assignment
#     (`=`), a `:=` bind writes every index (no boundary truncation) and the
#     returned rvalue reflects each position's write-time value (tests 54-55).

plan 8;

# --- HyperWhatever hammer index ---
{
    my @a = 0,[1,[2,[3,[4,[5,[6,7,8,9]]]]]];
    is-deeply @a[**], [^10].List, 'hyperwhatever hammer flattens all leaves';
}
{
    my @a = [1, [2, 3]], [[4], 5];
    is-deeply @a[**], (1, 2, 3, 4, 5), 'hyperwhatever hammer on mixed nesting';
}
{
    my @a = 1, 2, 3;
    is-deeply @a[**], (1, 2, 3), 'hyperwhatever hammer on a flat array is identity';
}

# --- Nested lazy slice bind (expression context) ---
{
    my @a = 11..15;
    is-deeply (@a[lazy 1,2,(4,5)] := "a"...*), ("a","b",("c","d")),
        'lazy slice bind 1: nested sublist keeps write-time values';
    is-deeply @a, [11,"a","b",14,"c","d"], 'lazy slice bind 1: final array';
}
{
    my @a = 11..15;
    is-deeply (@a[lazy 1,2,(4,5),4,5] := "a"...*), ("a","b",("c","d"),"e","f"),
        'lazy slice bind 2: duplicate indices are NOT truncated (bind)';
    is-deeply @a, [11,"a","b",14,"e","f"],
        'lazy slice bind 2: final array reflects last-write-wins for dup indices';
}

# --- Contrast: assignment (`=`) still truncates at the boundary ---
{
    my @a = 11..15;
    is-deeply (@a[lazy 1,2,(4,5),4,5] = "a"...*), ("a","b",("e","d"),"e"),
        'lazy slice assignment truncates and returns final-state values';
}
