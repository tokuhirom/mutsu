use v6;
use Test;

# An expression-position `my` declaration (`if (my $str = ...)`) creates a
# FRESH variable: a readonly marker left by a same-named parameter binding in
# a CALLER frame must not reject it, and the new variable stays writable
# (Text::IO::String's `print` declares `my Str $str` while called from
# `multi method new (Str $str!)`).

plan 4;

class TS {
    method print (*@what) {
        if (my $str = @what.join("")) {
            $str ~= "!";
            return $str;
        }
        "empty";
    }
    method mk (Str $str!) { self.print($str) }
}

is TS.mk("abc"), "abc!", 'callee if-cond `my` shadows caller readonly param';
is TS.mk(""), "empty", 'falsy branch unaffected';

class RO {
    method touch (Str $str!) {
        # The parameter itself stays readonly even after a nested call
        # declared (and unmarked) the same name in its own frame.
        self.print-like($str);
        try { $str = "nope" };
        $! ?? "still-ro" !! "mutated";
    }
    method print-like (*@what) {
        if (my $str = @what.join("")) { return $str }
        "";
    }
}
is RO.new.touch("x"), "still-ro", 'caller param mark restored after callee frame exits';

sub outer (Str $str!) {
    my $str2 = do if (my $str3 = $str ~ "y") { $str3 } else { "" };
    $str2;
}
is outer("x"), "xy", 'sub frame variant parses and runs';
