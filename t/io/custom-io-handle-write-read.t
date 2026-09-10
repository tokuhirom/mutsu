use Test;

# `Type/IO/Handle.rakudoc`'s "Creating Custom Handles" (6.d): a class
# `is IO::Handle` that implements `.WRITE` / `.READ` / `.EOF` gets the
# high-level text methods for free, because the base class routes them through
# those overrides.
#
# mutsu has that routing (`try_user_io_handle_method`), but it was wired into
# two of the interpreter's method-dispatch entry points only. The documented
# `$*OUT = $store` idiom goes through a third (an INTERNAL `.print` dispatch
# from `write_to_named_handle`), and the read-side methods go through a fourth
# (the mut path), so both fell through to the native `IO::Handle` arm and failed
# for want of a real file descriptor -- the redirect printed to the real stdout
# and captured nothing, and `.read` died "Expected IO::Handle".
#
# All expectations measured against raku v2026.07.

plan 6;

class Store is IO::Handle {
    has @.lines = [];
    submethod TWEAK { self.encoding: 'utf8' }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) { @!lines.push: data.decode; True }
}

# The documented redirect idiom.
{
    my $store = Store.new;
    my $old = $*OUT;
    $*OUT = $store;
    say "one";
    print "two\n";
    "three".say;
    $*OUT = $old;
    is $store.lines, ["one\n", "two\n", "three\n"],
        'a $*OUT redirect to a custom handle routes say/print through WRITE';
}


# Direct calls, which always worked, pinned so the routing cannot regress.
{
    my $store = Store.new;
    $store.print("a");
    $store.say("b");
    $store.put("c");
    is $store.lines, ["a", "b\n", "c\n"], 'print/say/put on a custom handle reach WRITE';
}

# The read side. A handle overriding BOTH WRITE and READ must still reach the
# read methods -- the write dispatch's catch-all used to bail out first.
class RW is IO::Handle {
    has $.data is rw = "";
    has $.pos is rw = 0;
    submethod TWEAK { self.encoding: 'utf8' }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) { $!data ~= data.decode; True }
    method READ(IO::Handle:D: Int:D \bytes --> Blob:D) {
        my $s = $!data.substr($!pos, bytes);
        $!pos += $s.chars;
        $s.encode('utf8')
    }
    method EOF { $!pos >= $!data.chars }
}

{
    my $h = RW.new;
    $h.print("onetwo");
    is $h.read(3).decode, 'one', '.read reaches READ';
    is $h.read(3).decode, 'two', 'and advances';
    ok $h.eof, '.eof reaches EOF';
}

{
    my $h = RW.new;
    $h.print("xy");
    is $h.getc, 'x', '.getc reaches READ';
}

# NOT about the routing above, and left in a file of its own for that reason:
# declaring ANY class with a `print` method makes an unrelated class's
# `$*OUT = $handle` redirect fall through to the real stdout. Measured
# 2026-09-06: the block below passes on its own and fails if a
# `class Cap { method print(*@a) {...} }` is declared anywhere in the same file,
# which is the shape of a name-keyed "a user overrode this native method" check
# that is not scoped to the receiver's class. See
# `todo/deep/custom-io-handle-write-read-not-dispatched.md`.
