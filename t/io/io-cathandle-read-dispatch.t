use Test;

plan 12;

# --- General fix 1: hyper method call with colon-arg syntax (»name: args) ---
my @joined = (('a', 'b'), ('c', 'd'))».join: '-';
is-deeply @joined, ['a-b', 'c-d'], 'hyper method call with colon-arg syntax';
my @subs = <foo bar baz>».substr: 0, 2;
is-deeply @subs, ['fo', 'ba', 'ba'],
    'hyper method colon-args with multiple positionals';

# --- General fix 2: .IO on an IO::Handle stored in an array element must
# dispatch the handle's .IO (returning its path), not stringify the handle. ---
{
    my $f = 'tmp/cathandle-dispatch-a.txt'.IO;
    $f.spurt('hi');
    my @h = ($f.open,);
    isa-ok @h[0].IO, IO::Path, '.IO on array-element handle returns an IO::Path';
    is @h[0].IO.absolute, $f.absolute,
        '.IO on array-element handle yields the open file path';
    @h[0].close;
}

# --- IO::CatHandle read methods across a mix of source types ---
{
    my $a = 'tmp/cathandle-a.txt'.IO; $a.spurt("a\nb\nc");
    my $b = 'tmp/cathandle-b.txt'.IO; $b.spurt("d\ne");
    my $c = 'tmp/cathandle-c.txt'.IO; $c.spurt("f\ng\nh");

    is-deeply IO::CatHandle.new($a, $b, $c).lines.List,
        <a b c d e f g h>.List, 'cat .lines reads across handles';

    is-deeply IO::CatHandle.new($a, $b, $c).lines(3).List,
        <a b c>.List, 'cat .lines($limit) honours the limit';

    is-deeply IO::CatHandle.new($a, $b, $c).lines(0).List,
        ().List, 'cat .lines(0) returns nothing';

    is IO::CatHandle.new($a, $b, $c).slurp, "a\nb\ncd\nef\ng\nh",
        'cat .slurp concatenates content';

    # comb with a string/regex matcher reads file content, not the object gist.
    is-deeply IO::CatHandle.new($a).comb(/\w/).List, <a b c>.List,
        'cat .comb(Regex) reads handle content';

    # split likewise.
    is-deeply IO::CatHandle.new($a).split("\n").List, <a b c>.List,
        'cat .split(Str) reads handle content';

    # .opened reflects active-handle presence.
    is-deeply IO::CatHandle.new.opened, False, 'empty cat is not opened';

    # .read with no count uses a sane default and yields a Buf[uint8].
    is-deeply IO::CatHandle.new($a).read, Buf[uint8].new("a\nb\nc".encode),
        'cat .read default size returns Buf[uint8]';
}
