use Test;

# `.fmt` of a single value with a wrong directive count dies with a plain
# X::AdHoc and a short message, where `sprintf` raises the typed
# X::Str::Sprintf::Directives::Count with the wrapped message and a single
# space before its hint. Expectations measured against rakudo 2026.09.

plan 10;

for ('3', 3, 3.5, 'x') -> $v {
    try $v.fmt("%s %s");
    is $!.^name, 'X::AdHoc', "{$v.^name}.fmt with too many directives is X::AdHoc";
    $! = Nil;
}

try 3.fmt("");
is $!.message, 'Your printf-style directives specify 0 arguments, but 1 argument was supplied',
    'a directive-less format on a scalar';
$! = Nil;

try sprintf("%s %s", 1);
is $!.^name, 'X::Str::Sprintf::Directives::Count', 'sprintf stays the typed exception';
is $!.message,
    "Your printf-style directives specify 2 arguments, but 1 argument was\nsupplied to format '%s %s'. Are you using an interpolated '\$'?",
    'with one space before the hint';
$! = Nil;

try (a => 1).fmt("%s %s %s");
is $!.^name, 'X::Str::Sprintf::Directives::Count', 'Pair.fmt is sprintf-backed';
try (1, 2).fmt("%s %s %s");
is $!.^name, 'X::AdHoc', 'List.fmt is X::AdHoc';

is bag(<a b a>).kxxv.sort.join(','), 'a,a,b', 'Bag.kxxv still lists each repeat';
