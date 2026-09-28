use Test;

# Placeholder signatures accept named args only through a matching placeholder
# or an implicit %_ slurpy read.

plan 8;

{
    sub one { $^x }
    my &call = &one;
    throws-like { call(1, language => 'html') }, X::AdHoc,
        message => "Unexpected named argument 'language' passed",
        'a placeholder sub rejects an unexpected named argument';
    is call(1), 1, 'the positional placeholder still binds';
}

{
    sub with-positionals { $^x; @_.elems }
    my &call = &with-positionals;
    throws-like { call(1, language => 'html') }, X::AdHoc,
        message => "Unexpected named argument 'language' passed",
        'reading @_ does not accept unexpected named arguments';
}

{
    sub with-named { $^x; %_.raku }
    is with-named(1, language => 'html'), '{:language("html")}',
        'reading %_ accepts and captures named arguments';
}

{
    my $block = { $^x };
    throws-like { $block(1, language => 'html') }, X::AdHoc,
        message => "Unexpected named argument 'language' passed",
        'a placeholder block rejects an unexpected named argument';
}

{
    my $block = { $^x; %_.raku };
    is $block(1, language => 'html'), '{:language("html")}',
        'a block reading %_ captures named arguments';
}

{
    sub named-placeholder { $^x; $:language }
    is named-placeholder(1, language => 'html'), 'html',
        'a named placeholder consumes its own argument';
    my &call = &named-placeholder;
    throws-like { call(1, language => 'html', extra => 2) }, X::AdHoc,
        message => "Unexpected named argument 'extra' passed",
        'a named placeholder does not consume an unrelated argument';
}
