use Test;

# `.split` builds each string separator's `:v` value once per call and shares
# it between matches, and moves each piece into its Str instead of copying it
# (#19201). These pin what `:v`/`:kv`/`:p` report for the string,
# string-list and regex splitter shapes that path touches.

plan 11;

is-deeply "a,b,,c".split(",", :v).List, ("a", ",", "b", ",", "", ",", "c"), 'string :v';
is-deeply "a,b,,c".split(",", :kv).List, ("a", 0, ",", "b", 0, ",", "", 0, ",", "c"), 'string :kv';
is-deeply "a,b,c".split(",", :p).List, ("a", 0 => ",", "b", 0 => ",", "c"), 'string :p';
is-deeply "a,b,c".split(",", 2, :v).List, ("a", ",", "b,c"), 'limit with :v';
is-deeply "abc".split("", :v).List, ("", "a", "b", "c", ""), 'empty separator reports no separators';
is-deeply "a😀b😀c".split("😀", :v).List, ("a", "😀", "b", "😀", "c"), 'non-ASCII separator';
is-deeply "a-b_c".split(<- _>, :v).List, ("a", "-", "b", "_", "c"), 'string list :v';
is-deeply "a-b_c".split(<- _>, :kv).List, ("a", 0, "-", "b", 1, "_", "c"), 'string list :kv';
is-deeply split(",", "x,y", :v).List, ("x", ",", "y"), 'split function :v';

my @rx = "a1b22c".split(/\d+/, :v);
is @rx.map(*.^name).join(","), "Str,Match,Str,Match,Str", 'regex :v reports Match objects';
is @rx[3].Str, "22", 'regex separator Match carries its text';

