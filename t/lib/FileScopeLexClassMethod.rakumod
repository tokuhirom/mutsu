unit module FileScopeLexClassMethod;
my $R = 9;
my class R { method r { $R ~ R.^name ~ R.new.WHAT.^name } }
sub make-r is export { R.new }
