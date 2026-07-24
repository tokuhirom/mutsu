unit package URI::Escape;

use IETF::RFC_Grammar::URI;

my %escapes = (^256).flatmap: {
    .chr => sprintf '%%%02X', $_
};

# line below may work and be useful when fix RT #126252
#   my token escape_unreserved {<IETF::RFC_Grammar::URI::unreserved>};

multi sub uri-escape($s, Bool :$no-utf8 = False) is export {
    $s // return $s;
    $s.subst(:g, /<- [\-._~A..Za..z0..9]>/,
        {
            ( $no-utf8 || .Str.ord < 128 ) ?? %escapes{ .Str } !!
                .Str.encode.list.fmt('%%%X', "")
        }
    );
}

multi sub uri-escape(Match $s, Bool :$no-utf8 = False) is export {
    uri-escape( $s.Str, :$no-utf8 );
}

# todo - automatic invalid UTF-8 detection
# see http://www.w3.org/International/questions/qa-forms-utf-8
#     find first sequence of %[89ABCDEF]<.xdigit>
#         use algorithm from url to determine if it's valid UTF-8
sub uri-unescape(*@to_unesc, Bool :$no-utf8 = False, Str :$enc = 'UTF-8') is export {
    my @rc;
    if $no-utf8 or $enc eq 'UTF-8' {
        @rc = @to_unesc.flatmap: {
            .trans('+' => ' ')\
            .subst(:g, / [ '%' (<.xdigit> ** 2 ) ]+ /, -> $/ {
                $no-utf8
                    ?? $0.flatmap({ :16(~$_).chr }).join
                    !! Buf.new($0.flatmap({ :16(~$_) })).decode('UTF-8')
            });
        }
    }
    else {
        my $Buf = Buf.new;
        for @to_unesc -> $uri is copy {
            $uri .= trans('+' => ' ');
            my $index = 0;
            while $uri.index('%', $index) {
                my $new-index = $uri.index('%', $index);
                $Buf.push($uri.substr($index, $new-index - $index).encode($enc));
                $Buf.push(:16(~$0)) if $uri.substr($new-index + 1, 2) ~~ / ( <:AHex> ** 2 ) /;
                $index = $new-index + 3;
            }
            $Buf.push( $uri.substr($index, $uri.chars - $index).encode($enc))
                if $index < $uri.chars;
            @rc.push($Buf.decode($enc));
        }
    }
    return |@rc;
}

# preserve snake case interface
sub uri_escape($s, Bool :$no_utf8 = False) is export {
    uri-escape($s, no-utf8 => $no_utf8)
}

sub uri_unescape(*@to_unesc, Bool :$no_utf8 = False) is export {
    uri-unescape(@to_unesc, no-utf8 => $no_utf8)
}

# vim: expandtab shiftwidth=4
