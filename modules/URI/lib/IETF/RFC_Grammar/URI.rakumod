# Taken/Copied with relatively minor translation to Raku
# from RFC 3986 (http://www.ietf.org/rfc/rfc3986.txt)

# Rule names moved from snake case with _ to kebab case with -
# 9/2015.  The rfc grammar is specified in kebab case.

use IETF::RFC_Grammar::IPv6;

unit grammar IETF::RFC_Grammar::URI:ver<0.3.8> is IETF::RFC_Grammar::IPv6;

token TOP               { <URI-reference> };
token TOP-non-empty     { <URI> | <relative-ref-non-empty> };
token URI-reference     { <URI> | <relative-ref> };

token absolute-URI      { <scheme> ':' <.hier-part> [ '?' query ]? };

token relative-ref      {
    <relative-part> [ '?' <query> ]? [ '#' <fragment> ]?
};
token relative-part     {
    | <?[/]> '//' <authority> <path-abempty>
    | <path-absolute>
    | <path-noscheme>
    | <path-empty>
};

token relative-ref-non-empty      {
    <relative-part-non-empty> [ '?' <query> ]? [ '#' <fragment> ]?
};
token relative-part-non-empty     {
    | <?[/]> '//' <authority> <path-abempty>
    | <path-absolute>
    | <path-noscheme>
};

token URI               {
    <scheme> ':' <hier-part> ['?' <query> ]?  [ '#' <fragment> ]?
};

token hier-part     {
    | '//' <authority> <path-abempty>
    | <path-absolute>
    | <path-rootless>
    | <path-empty>
};

token scheme            { <.uri-alpha> <[\-+.] +uri-alpha +digit>* };

token authority         { [ <userinfo> '@' ]? <host> [ ':' <port> ]? };
token userinfo          {
    [ ':' | <likely-userinfo-component> ]*
};
# the rfc refers to username:password as deprecated
token likely-userinfo-component {
    <+unreserved +sub-delims>+ | <.pct-encoded>+
};
token host              { <IPv4address> | <IP-literal> | <reg-name> };
token port              { <.digit>* };

token IP-literal        { '[' [ <IPv6address> | <IPvFuture> ] ']' };
token IPvFuture         {
    'v' <.xdigit>+ '.' <[:] +unreserved +sub-delims>+
};
token reg-name          { [ <+unreserved +sub-delims> | <.pct-encoded> ]* };

token path-abempty      { [ '/' <segment> ]* };
token path-absolute     { '/' [ <segment-nz> [ '/' <segment> ]* ]? };
token path-noscheme     { <segment-nz-nc> [ '/' <segment> ]* };
token path-rootless     { <segment-nz> [ '/' <segment> ]* };
token path-empty        { <.pchar> ** 0 }; # yes - zero characters

token segment         { <.pchar>* };
token segment-nz      { <.pchar>+ };
token segment-nz-nc   { [ <+unenc-pchar - [:]> | <.pct-encoded> ] + };

token query             { <.fragment> };
token fragment          { [ <[/?] +unenc-pchar> | <.pct-encoded> ]* };

token pchar             { <.unenc-pchar> | <.pct-encoded> };
token unenc-pchar       { <[:@] +unreserved +sub-delims> };

token pct-encoded       { '%' <.xdigit> <.xdigit> };

token unreserved        { <[\-._~] +uri-alphanum> };

token reserved          { <+gen-delims +sub-delims> };

token gen-delims        { <[:/?\#\[\]@]> };
token sub-delims        { <[;!$&'()*+,=]> };

token uri-alphanum      { <+uri-alpha +:N +:S> };   
token uri-alpha         { <alpha> };

# vim: expandtab shiftwidth=4
