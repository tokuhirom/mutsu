use Test;

# A qualified method call `self.Builtin::method` where the qualifier is a NATIVE
# builtin ancestor (e.g. `IO::Path`) must dispatch to that type's native method,
# bypassing any override on the derived class. Previously mutsu could only resolve
# a qualified call to a *user*-defined ancestor method: `self.IO::Path::slurp`
# from a class that `is IO::Path` failed with "No such method 'IO::Path::slurp'"
# (instance) or "Cannot dispatch to a method on IO" (run-time mixin, mis-split at
# the first `::`).

plan 6;

# A temp file to slurp/lines against.
my $file = $*TMPDIR.add("qnam-{$*PID}.txt");
$file.spurt("alpha\nbeta\ngamma\n");
LEAVE { $file.unlink if $file.e }

# --- subclass of a builtin: qualified call to the builtin ancestor's method ---
{
    my class MyPath is IO::Path {
        method myslurp { self.IO::Path::slurp(:enc<utf8>) }
        method mylines { self.IO::Path::lines(:enc<utf8>) }
    }
    my $p = MyPath.new($file.absolute);
    is $p.myslurp, "alpha\nbeta\ngamma\n", 'self.IO::Path::slurp on a subclass instance';
    is-deeply $p.mylines.List, <alpha beta gamma>.List, 'self.IO::Path::lines on a subclass instance';
}

# --- overriding method calls the ancestor's native version (no recursion) ---
{
    my class Loud is IO::Path {
        method slurp(|c) { "LOUD:" ~ self.IO::Path::slurp(|c) }
    }
    my $p = Loud.new($file.absolute);
    is $p.slurp(:enc<utf8>), "LOUD:alpha\nbeta\ngamma\n",
        'overriding slurp delegating to self.IO::Path::slurp does not recurse';
}

# --- run-time mixin over a builtin-subclass instance (the AutoDecompress shape) ---
{
    my role Proccer { method fetch { self.IO::Path::slurp(:enc<utf8>) } }
    my class MyPath2 is IO::Path {}
    my $p = MyPath2.new($file.absolute) but Proccer;
    is $p.fetch, "alpha\nbeta\ngamma\n", 'self.IO::Path::slurp through a run-time mixin';
}

# --- a bad qualifier (not in the MRO) is still rejected ---
{
    my class Plain is IO::Path {}
    my $p = Plain.new($file.absolute);
    dies-ok { $p.Str::slurp }, 'qualifier not in the MRO still dies';
}

# --- a user-class ancestor qualified call still works (regression) ---
{
    my class Base { method greet { 'base' } }
    my class Deriv is Base { method greet { 'deriv:' ~ self.Base::greet } }
    is Deriv.new.greet, 'deriv:base', 'user-class ancestor qualified call still works';
}
