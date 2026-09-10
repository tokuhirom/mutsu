use v6;
use Test;

# A user-defined method shadows an inherited NATIVE method: for a user
# subclass of a builtin (`class S is IO::Handle`), the base class's native
# method set is found via the MRO, but the subclass's own method is more
# derived and must win. `~$fh` on a Text::IO::String stringified through the
# native IO::Handle Str ("IO::Handle()") instead of the user `method Str`
# (Text::CSV 46_eol_si), which broke its close-time writeback
# (`$!str = ~self`).

plan 5;

class S is IO::Handle {
    has Str @!content;
    method add ($s) { @!content.push: $s }
    method Str  { @!content.join("") }
    method gist { "S[" ~ @!content.elems ~ "]" }
}

my $s = S.new;
$s.add("hel");
$s.add("lo");

is $s.Str, "hello", "explicit .Str calls the user method";
is ~$s, "hello", "prefix ~ dispatches the user Str, not the native IO::Handle one";
is "$s", "hello", "string interpolation dispatches the user Str";
is $s.gist, "S[2]", "user gist shadows the native gist";

# The 46_eol_si shape: assignment writes ~self back through a bound Str
class Sink is IO::Handle {
    has Str $!str;
    method bind-str (Str $s is rw) { $!str := $s }
    method fill ($v) { $!str = $v }
    method Str { "CONTENT" }
    method close-like { $!str.defined and $!str = ~ self }
}
my Str $out = "";
my $k = Sink.new;
$k.bind-str($out);
$k.close-like;
is $out, "CONTENT", "~self inside a method uses the user Str";
