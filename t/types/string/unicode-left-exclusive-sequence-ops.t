use Test;

# The Unicode spellings of the left-exclusive sequence operators `^...` and
# `^...^` are sequence operators too (#14913; fixed by the sequence-operator
# scanner in #15206, pinned here).
plan 6;

is (1 ^... 5).List, (2, 3, 4, 5), 'ASCII ^... drops the first element';
is (1 ^...^ 5).List, (2, 3, 4), 'ASCII ^...^ drops both ends';
is (1 ^… 5).List, (2, 3, 4, 5), 'Unicode ^… behaves as ^...';
is (1 ^…^ 5).List, (2, 3, 4), 'Unicode ^…^ behaves as ^...^';
is (1, 2 ^… 5).List, (2, 3, 4, 5), 'an explicit seed list with ^…';
is (1 ^… *).head(3).List, (2, 3, 4), 'a lazy ^… sequence';
