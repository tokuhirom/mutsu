use Test;

plan 5;

# `given`/`with`/`without` alias `$_` to a nested element, autovivifying the
# intermediate containers.
{
    my %f;
    $_ = 7 without %f<a>[0];
    is %f.raku, '{:a($[7])}', 'statement-modifier without on a nested element';
}

{
    my @n;
    given @n[1][2] { $_ = 5 }
    is @n.raku, '[Any, [Any, Any, 5]]', 'given on a nested array element';
}

{
    my %h;
    given %h<x><y> { $_ = 'v' }
    is %h.raku, '{:x(${:y("v")})}', 'given on a nested hash element';
}

class C {
    has %!h;
    has @!a;
    method m {
        $_ = [] without %!h<k>[0];
        $_ = 3 without @!a[1];
        %!h.raku ~ ' ' ~ @!a.raku
    }
}
is C.new.m, '{:k($[[],])} [Any, 3]', 'attribute containers alias the element';

{
    my %f = a => [1];
    $_ = 9 without %f<a>[0];
    is %f<a>[0], 1, 'a defined element is left alone';
}
