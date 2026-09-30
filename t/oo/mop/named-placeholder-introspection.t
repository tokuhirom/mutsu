use Test;

plan 10;

my &block = { $:begin };
is &block.signature.raku, ':(:$begin!)', 'named placeholder renders as a required named parameter';
is-deeply &block.signature.params[0].named_names.List, ('begin',),
    'named_names omits the placeholder twigil';
is &block.signature.params[0].name, '$begin', 'parameter name uses the variable spelling';

my %available = begin => 7, ignored => 9;
my %selected = %available.grep({ .key eq &block.signature.params[0].named_names[0] }).Hash;
is block(|%selected), 7, 'signature-selected named argument reaches the placeholder';

my &callable = { &:callback };
is &callable.signature.raku, ':(:&callback!)', 'callable named placeholder renders as required';
is-deeply &callable.signature.params[0].named_names.List, ('callback',),
    'callable named placeholder exposes its argument key';
is &callable.signature.params[0].name, '&callback', 'callable parameter preserves its sigil';

sub named-sub { $:begin }
is &named-sub.signature.raku, ':(:$begin!)', 'implicit sub signature renders named placeholder';
is-deeply &named-sub.signature.params[0].named_names.List, ('begin',),
    'implicit sub signature exposes its argument key';
is named-sub(|%selected), 7, 'signature-selected argument also binds in a sub';
