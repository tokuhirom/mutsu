use v6;
use Test;
use nqp;

# Three gaps that blocked Text::Diff::Sift4 (T-035):
#  1. `nqp::ordat($str, $pos)` was unimplemented ("Unknown function: ordat").
#  2. A typed scalar `:=`-bound to an array element (`my Offset $o := @a[$i]`)
#     type-checked the ContainerRef itself and reported the bogus "got Any".
#  3. A Bool assigned to a native `int` was rejected instead of coercing (Bool
#     `does Int`, so True=1 / False=0).

plan 11;

# --- 1. nqp::ordat -------------------------------------------------------
is nqp::ordat("abc", 0), 97,  'nqp::ordat first char';
is nqp::ordat("abc", 2), 99,  'nqp::ordat later char';
is nqp::ordat("h\c[LATIN SMALL LETTER E WITH ACUTE]llo", 1), 233,
    'nqp::ordat non-ASCII codepoint';
is nqp::ordat("abc", 5), -1,  'nqp::ordat past end returns -1';

# --- 2. typed scalar := bound to an array element ------------------------
{
    class Offset {
        has int $.c1 is built(:bind);
        has int $.c2 is built(:bind);
    }
    my @offset_arr;
    @offset_arr.push(Offset.new(c1 => 3, c2 => 5));
    my int $i = 0;
    my Offset $ofs;
    $ofs := @offset_arr[$i];
    is $ofs.c1, 3, 'typed scalar bind to array element reads .c1';
    is $ofs.c2, 5, 'typed scalar bind to array element reads .c2';
    is $ofs.^name, 'Offset', 'bound typed scalar has the element type';
}

# --- 3. Bool coerces to a native int ------------------------------------
{
    my int $x;
    $x = (3 >= 2);
    is $x, 1, 'True assigned to native int stores 1';
    is $x.^name, 'Int', 'native int holds an Int, not a Bool';
    $x = (1 >= 2);
    is $x, 0, 'False assigned to native int stores 0';
    my int $y = True;
    is $y, 1, 'native int declared with a Bool default is 1';
}
