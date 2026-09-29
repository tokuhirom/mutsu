use Test;
use MONKEY-TYPING;

# From the FatRatStr distribution: the allomorph types are core classes and
# can be augmented.

plan 3;

augment class NumStr { method tag { "numstr" } }
augment class IntStr { method tag { "intstr" } }
augment class RatStr { method tag { "ratstr" } }

is <1e0>.tag, "numstr", 'augment NumStr';
is <1>.tag, "intstr", 'augment IntStr';
is <1.5>.tag, "ratstr", 'augment RatStr';
