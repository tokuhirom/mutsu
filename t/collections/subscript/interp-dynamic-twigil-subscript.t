use Test;

# A twigilled array/hash variable followed by a subscript interpolates in a
# double-quoted string, exactly like the twigil-less form (`"@a[0]"`).
# The `*` (dynamic) twigil used to be left literal: `"@*ARGS[0]"`.

plan 10;

{
    my @*ARGS = <x y>;
    is "v=@*ARGS[0]", 'v=x', '@*ARGS[0] interpolates';
    is "v=@*ARGS[1,0]", 'v=y x', '@*ARGS slice interpolates';
    is "v=@*ARGS[]", 'v=x y', '@*ARGS[] zen slice interpolates';
    is "v=@*ARGS.join(',')", 'v=x,y', '@*ARGS.method() interpolates';
    is "plain @*ARGS here", 'plain @*ARGS here', 'bare @*ARGS stays literal';
}

{
    my %*H = k => 'bar';
    is "e=%*H<k>", 'e=bar', '%*H<k> interpolates';
    is "e=%*H{'k'}", 'e=bar', "%*H\{'k'} interpolates";
    is "mail foo%*H here", 'mail foo%*H here', 'bare %*H stays literal';
}

{
    class C {
        has @!a = 5, 6;
        has %!h = k => 7;
        method m { "a=@!a[0] h=%!h<k>" }
    }
    is C.new.m, 'a=5 h=7', 'attribute @!a[0] / %!h<k> still interpolate';
}

is "foo@*bar", 'foo@*bar', 'a @* not followed by a subscript is literal';
