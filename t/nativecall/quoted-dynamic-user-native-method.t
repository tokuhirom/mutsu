use Test;

plan 1;

# Found by PrettyDump 1.2.3: a quoted dynamic method name must let a user
# method shadow the native method with the same name.
class Target {
    method Str(Str:D $value --> Str) { "user:" ~ $value.raku }

    method render {
        my $name = 'Str';
        self."$name"('value')
    }
}

is Target.new.render, 'user:"value"',
    'quoted dynamic method dispatch keeps the user method before native Str';
