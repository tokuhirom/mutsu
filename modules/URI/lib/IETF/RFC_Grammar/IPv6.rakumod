# Taken/Copied with relatively minor translation to Raku
# from RFC 3986 (http://www.ietf.org/rfc/rfc3986.txt)

unit grammar IETF::RFC_Grammar::IPv6:ver<0.3.8>;

token IPv6address      {
                                            [ <.h16> ':' ] ** 6 <.ls32> |
                                       '::' [ <.h16> ':' ] ** 5 <.ls32> |
    [                        <.h16> ]? '::' [ <.h16> ':' ] ** 4 <.ls32> |
    [ [ <.sep-h16> ]?        <.h16> ]? '::' [ <.h16> ':' ] ** 3 <.ls32> |
    [ [ <.sep-h16> ] ** 0..2 <.h16> ]? '::' [ <.h16> ':' ] ** 2 <.ls32> |        
    [ [ <.sep-h16> ] ** 0..3 <.h16> ]? '::' <.h16> ':'          <.ls32> |
    [ [ <.sep-h16> ] ** 0..4 <.h16> ]? '::'                     <.ls32> |
    [ [ <.sep-h16> ] ** 0..5 <.h16> ]? '::'                     <.h16>  |
    [ [ <.sep-h16> ] ** 0..6 <.h16> ]? '::'                                      
};

# token avoiding backtracking happiness    
token sep-h16           { [ <.h16> ':' <!before ':'>] }

token ls32              { [<.h16> ':' <.h16>] | <.IPv4address> };
token h16               { <.xdigit> ** 1..4 };
    
token IPv4address       {
    <.dec-octet> '.' <.dec-octet> '.' <.dec-octet> '.' <.dec-octet>
};
    
token dec-octet         {
    '25' <[0..5]>           |   # 250 - 255
    '2' <[0..4]> <.digit>   |   # 200 - 249
    '1' <.digit> ** 2       |   # 100 - 199
    <[1..9]> <.digit>       |   # 10 - 99
    <.digit>                    # 0 - 9
}
    
# vim: expandtab shiftwidth=4
