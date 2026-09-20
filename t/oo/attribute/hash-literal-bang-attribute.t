use Test;

plan 1;

class PathBox {
    has $.path is rw;

    method clone {
        nextwith :path({ :$!path });
    }
}

is PathBox.new(path => 'tmp').clone.path<path>, 'tmp',
    'a hash literal may initialize a value from a private attribute';
