use Test;

# A user-defined IO::Handle subclass that overrides WRITE/READ/EOF: the
# high-level methods (print/say/.../slurp/lines/get/...) dispatch through the
# user methods, with no underlying OS file. Regression for roast
# S32-io/io-handle.t `.WRITE` and `.EOF/.WRITE` subtests.

plan 15;

# --- Output via user WRITE ---------------------------------------------------
{
    my $fh := my class MyOut is IO::Handle {
        has Buf[uint8] $.data .= new;
        submethod TWEAK { self.encoding: 'utf8' }
        method WRITE (Blob:D \data --> True) { $!data.append: data }
    }.new;

    $fh.print:  'print ';
    $fh.printf: 'pri%s', 'ntf ';
    $fh.put:    'put';
    $fh.say:    my class { method gist { 'say' } }.new;
    $fh.spurt:  'spurt text ';
    $fh.spurt:  'spurt bin '.encode;
    $fh.write:  'write'.encode;
    $fh.print-nl;

    is-deeply $fh.data.decode.lines,
        ('print printf put', 'say', 'spurt text spurt bin write'),
        'all writing methods route through user WRITE';
}

# --- Input via user READ/EOF -------------------------------------------------
my $make = my class MyIn is IO::Handle {
    has Buf[uint8] $.data .= new: "I ♥ Raku\nprogramming".encode;
    submethod TWEAK { self.encoding: 'utf8' }
    method READ(\bytes) { $!data.splice: 0, bytes }
    method EOF { ! $!data }
};

{
    my $fh := $make.new;
    is-deeply $fh.eof,   False, 'eof before read';
    is-deeply $fh.slurp, "I ♥ Raku\nprogramming", '.slurp via READ';
    is-deeply $fh.eof,   True,  'eof after slurp';
}

{
    my $fh := $make.new;
    is-deeply $fh.lines, ('I ♥ Raku', 'programming').Seq, '.lines via READ';
    is-deeply $fh.eof,   True, 'eof after lines';
}

{
    my $fh := $make.new;
    $fh.nl-in = [<ak gra>];
    is-deeply $fh.lines, ('I ♥ R', "u\npro", 'mming').Seq,
        '.lines with custom nl-in';
}

{
    my $fh := $make.new;
    is-deeply $fh.words, <I ♥ Raku programming>».Str.Seq, '.words via READ';
}

{
    my $fh := $make.new;
    is-deeply $fh.split(/ak/), ("I ♥ R", "u\nprogramming").Seq, '.split via READ';
}

{
    my $fh := $make.new;
    is-deeply $fh.get,  'I ♥ Raku',    'first .get reads one line';
    is-deeply $fh.eof,  False,         'eof: data remains after first get';
    is-deeply $fh.get,  'programming', 'second .get reads the rest';
    is-deeply $fh.get,  Nil,           'third .get returns Nil at EOF';
}

{
    my $fh := $make.new;
    is-deeply ($fh.getc xx 5), ('I', ' ', '♥', ' ', 'R').Seq, 'five .getc calls';
}

{
    my $fh := $make.new;
    is-deeply $fh.read(7).decode, "I ♥ R", '.read(7) returns 7 bytes';
}
