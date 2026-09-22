unit module P5substr;

proto sub substr(|) is export {*}

multi sub substr(Str:D \s, Int:D $o is copy, Int:D $l is copy = s.chars - $o) is rw is export {
    Proxy.new(
        FETCH => -> $ {
            s.substr($o, $l)
        },
        STORE => -> $, \new {
            s.substr-rw($o, $l) = new;
            $l = new.chars;
        },
    )
}
