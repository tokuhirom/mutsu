use Test;

# A `.`-twigil variable's name is a longname, so `$.numeric:sym<frac>($/)`
# calls the method declared as `method numeric:sym<frac>`. From
# PDF::Grammar's actions (`$<frac> ?? $.numeric:sym<frac>($/) !! ...`).

plan 3;

class A {
    method m:sym<a>($x) { "a:$x" }
    method m:sym<b>()   { 'b' }
    method go     { $.m:sym<a>(5) }
    method go-b   { $.m:sym<b> }
    method plain  { $.go }
}

is A.go, 'a:5', '$.name:sym<x>(args) calls the longname method';
is A.go-b, 'b', 'without an argument list';
is A.plain, 'a:5', 'a plain $.name still calls name';
