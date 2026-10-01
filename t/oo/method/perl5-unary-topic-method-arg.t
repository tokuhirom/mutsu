use Test;

# From the PublicSuffix distribution: `lc .contains('xn--') ?? ... !! ...`.
# After whitespace, a `.method` term is the argument of ord/chr/lc/uc/abs
# (the topic's method call), not a bare use of the routine.

plan 6;

for <ABC> {
    is (lc .contains("ab")), "false", 'lc .method takes the topic method call';
    is (lc .contains("AB")), "true", 'lc .method result is lc of the call';
    is (uc .lc), "ABC", 'uc .method';
    is (abs .chars), 3, 'abs .method';
    is (chr .ord), "A", 'chr .method';
}

throws-like { EVAL 'ord.Cool' }, X::Obsolete, 'tight `ord.Cool` is still a bare use';
