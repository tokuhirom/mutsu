use Test;
use lib 't/lib';
use LexsubFrameMain;

# A routine declared in a `supply` block and called from its `whenever` body
# runs in the module that declares it: the module's own `use` of another module
# makes a qualified call into it visible, though this script never `use`s it.
plan 2;

my @got;
react {
    whenever LexsubFrameMain.listen(Supply.from-list(1, 2)) -> $v { @got.push($v) }
}
is-deeply @got, [41, 42], 'a block-declared routine sees its own module imports';

my @second;
react {
    whenever LexsubFrameMain.listen(Supply.from-list(10)) -> $v { @second.push($v) }
}
is-deeply @second, [50], 'and still does on a second tap';
