my @codes;

class HTTP::Status {
    has int $.code    is required;
    has str $.title   is required;
    has str $.summary is required;
    has str $.origin;
    has str $.since;
    has str $.RFC;

    multi method new(Int() :$code, Str() :$RFC) {
        self.bless(:$code, :$RFC, |%_)
    }
    multi method new($code, $title, $summary) {
        self.bless(:$code, :$title, :$summary, |%_)
    }
    submethod TWEAK(:$origin, :$since, :$RFC --> Nil) {
        $!origin = 'IANA'     unless $origin;
        $!since  = 'HTTP/1.0' unless $since;
        $!RFC    =  '7231'    unless $RFC || $origin; 
    }
    method sink() {  # bare creation updates list
        @codes[$!code] = self;
    }

    method Int()     { $!code }
    method Numeric() { $!code }
    method Str()     { $!title }
    method gist()    { $!title }

    method type() {
        $!code < 100
          ?? Nil
          !! $!code < 200
            ?? "Informational"
            !! $!code < 300
              ?? "Success"
              !! $!code < 400
                ?? "Redirection"
                !! $!code < 500
                  ?? "Client Error"
                  !! $!code < 600
                    ?? "Server Error"
                    !! Nil
    }

    method source() {
        "https://en.wikipedia.org/w/index.php?title=List_of_HTTP_status_codes&oldid=1056056186"
    }

    method CALL-ME(HTTP::Status:U: Int() $code) { @codes[$code] // Nil }

    method pairs(HTTP::Status:U:)  { @codes[]:p  }
    method kv(HTTP::Status:U:)     { @codes[]:kv }
    method keys(HTTP::Status:U:)   { @codes[]:k  }
    method values(HTTP::Status:U:) { @codes[]:v  }

    method Map(HTTP::Status:U:)  {  Map.new: @codes[]:p }
    method Hash(HTTP::Status:U:) { Hash.new: @codes[]:p }
}

BEGIN {
    HTTP::Status.new: 100, 'Continue',
    Q§The server has received the request headers and the client should proceed to send the request body (in the case of a request for which a body needs to be sent; for example, a POST request). Sending a large request body to a server after a request has been rejected for inappropriate headers would be inefficient. To have a server check the request's headers, a client must send Expect: 100-continue as a header in its initial request and receive a 100 Continue status code in response before sending the body. If the client receives an error code such as 403 (Forbidden) or 405 (Method Not Allowed) then it should not send the request's body. The response 417 Expectation Failed indicates that the request should be repeated without the Expect header as it indicates that the server does not support expectations (this is the case, for example, of HTTP/1.0 servers).§;

    HTTP::Status.new: 101, 'Switching Protocols',
    Q§The requester has asked the server to switch protocols and the server has agreed to do so.§;

    HTTP::Status.new: 102, 'Processing (WebDAV)',
    Q§A WebDAV request may contain many sub-requests involving file operations, requiring a long time to complete the request. This code indicates that the server has received and is processing the request, but no response is available yet. This prevents the client from timing out and assuming the request was lost.§, :RFC<2518>;

    HTTP::Status.new: 103, 'Early Hints',
    Q§Used to return some response headers before final HTTP message.§, :RFC<8297>;

    HTTP::Status.new: 200, 'OK',
    Q§Standard response for successful HTTP requests. The actual response will depend on the request method used. In a GET request, the response will contain an entity corresponding to the requested resource. In a POST request, the response will contain an entity describing or containing the result of the action.§;

    HTTP::Status.new: 201, 'Created',
    Q§The request has been fulfilled, resulting in the creation of a new resource.§;

    HTTP::Status.new: 202, 'Accepted',
    Q§The request has been accepted for processing, but the processing has not been completed. The request might or might not be eventually acted upon, and may be disallowed when processing occurs.§;

    HTTP::Status.new: 203, 'Non-Authoritative Information',
    Q§The server is a transforming proxy (e.g. a Web accelerator) that received a 200 OK from its origin, but is returning a modified version of the origin's response.§, :since<HTTP/1.1>;

    HTTP::Status.new: 204, 'No Content',
    Q§The server successfully processed the request, and is not returning any content.§;

    HTTP::Status.new: 205, 'Reset Content',
    Q§The server successfully processed the request, asks that the requester reset its document view, and is not returning any content.§;

    HTTP::Status.new: 206, 'Partial Content',
    Q§The server is delivering only part of the resource (byte serving) due to a range header sent by the client. The range header is used by HTTP clients to enable resuming of interrupted downloads, or split a download into multiple simultaneous streams.§, :RFC<7233>;

    HTTP::Status.new: 207, 'Multi-Status (WebDAV)',
    Q§The message body that follows is by default an XML message and can contain a number of separate response codes, depending on how many sub-requests were made.§, :RFC<4918>;

    HTTP::Status.new: 208, 'Already Reported (WebDAV)',
    Q§The members of a DAV binding have already been enumerated in a preceding part of the (multistatus) response, and are not being included again.§, :RFC<5842>;

    HTTP::Status.new: 218, 'This is fine',
    Q§Used as a catch-all error condition for allowing response bodies to flow through Apache when ProxyErrorOverride is enabled. When ProxyErrorOverride is enabled in Apache, response bodies that contain a status code of 4xx or 5xx are automatically discarded by Apache in favor of a generic response or a custom response specified by the ErrorDocument directive. The phrase "This is fine" is an Internet meme referring to ignoring the situation or taking no action despite obvious evidence of an ongoing catastrophe.§, :origin('Apache Web Server');

    HTTP::Status.new: 226, 'IM Used',
    Q§The server has fulfilled a request for the resource, and the response is a representation of the result of one or more instance-manipulations applied to the current instance.§, :RFC<3229>;

    HTTP::Status.new: 300, 'Multiple Choices',
    Q§Indicates multiple options for the resource from which the client may choose (via agent-driven content negotiation). For example, this code could be used to present multiple video format options, to list files with different filename extensions, or to suggest word-sense disambiguation.§;

    HTTP::Status.new: 301, 'Moved Permanently',
    Q§This and all future requests should be directed to the given URI.§;

    HTTP::Status.new: 302, 'Found (Previously "Moved temporarily")',
    Q§Tells the client to look at (browse to) another URL. The HTTP/1.0 specification (RFC 1945) required the client to perform a temporary redirect with the same method (the original describing phrase was "Moved Temporarily"), but popular browsers implemented 302 redirects by changing the method to GET. Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish between the two behaviours.§;

    HTTP::Status.new: 303, 'See Other',
    Q§The response to the request can be found under another URI using the GET method. When received in response to a POST (or PUT/DELETE), the client should presume that the server has received the data and should issue a new GET request to the given URI.§, :since<HTTP/1.1>;

    HTTP::Status.new: 304, 'Not Modified',
    Q§Indicates that the resource has not been modified since the version specified by the request headers If-Modified-Since or If-None-Match. In such case, there is no need to retransmit the resource since the client still has a previously-downloaded copy.§, :RFC<7232>;

    HTTP::Status.new: 305, 'Use Proxy',
    Q§The requested resource is available only through a proxy, the address for which is provided in the response. For security reasons, many HTTP clients (such as Mozilla Firefox and Internet Explorer) do not obey this status code.§, :since<HTTP/1.1>;

    HTTP::Status.new: 306, 'Switch Proxy',
    Q§No longer used. Originally meant "Subsequent requests should use the specified proxy".§;

    HTTP::Status.new: 307, 'Temporary Redirect',
    Q§In this case, the request should be repeated with another URI; however, future requests should still use the original URI. In contrast to how 302 was historically implemented, the request method is not allowed to be changed when reissuing the original request. For example, a POST request should be repeated using another POST request.§, :since<HTTP/1.1>;

    HTTP::Status.new: 308, 'Permanent Redirect',
    Q§This and all future requests should be directed to the given URI. 308 parallel the behaviour of 301, but does not allow the HTTP method to change. So, for example, submitting a form to a permanently redirected resource may continue smoothly.§, :since<HTTP/1.1>;

    HTTP::Status.new: 400, 'Bad Request',
    Q§The server cannot or will not process the request due to an apparent client error (e.g., malformed request syntax, size too large, invalid request message framing, or deceptive request routing).§;

    HTTP::Status.new: 401, 'Unauthorized',
    Q§Similar to 403 Forbidden, but specifically for use when authentication is required and has failed or has not yet been provided. The response must include a WWW-Authenticate header field containing a challenge applicable to the requested resource. See Basic access authentication and Digest access authentication.[31] 401 semantically means "unauthorised",[32] the user does not have valid authentication credentials for the target resource.§, :RFC<7235>;

    HTTP::Status.new: 402, 'Payment Required',
    Q§Reserved for future use. The original intention was that this code might be used as part of some form of digital cash or micropayment scheme, as proposed, for example, by GNU Taler, but that has not yet happened, and this code is not widely used. Google Developers API uses this status if a particular developer has exceeded the daily limit on requests. Sipgate uses this code if an account does not have sufficient funds to start a call. Shopify uses this code when the store has not paid their fees and is temporarily disabled. Stripe uses this code for failed payments where parameters were correct, for example blocked fraudulent payments.§;

    HTTP::Status.new: 403, 'Forbidden',
    Q§The request contained valid data and was understood by the server, but the server is refusing action. This may be due to the user not having the necessary permissions for a resource or needing an account of some sort, or attempting a prohibited action (e.g. creating a duplicate record where only one is allowed). This code is also typically used if the request provided authentication by answering the WWW-Authenticate header field challenge, but the server did not accept that authentication. The request should not be repeated.§;

    HTTP::Status.new: 404, 'Not Found',
    Q§The requested resource could not be found but may be available in the future. Subsequent requests by the client are permissible.§;

    HTTP::Status.new: 405, 'Method Not Allowed',
    Q§A request method is not supported for the requested resource; for example, a GET request on a form that requires data to be presented via POST, or a PUT request on a read-only resource.§;

    HTTP::Status.new: 406, 'Not Acceptable',
    Q§The requested resource is capable of generating only content not acceptable according to the Accept headers sent in the request. See Content Negotiation.§;

    HTTP::Status.new: 407, 'Proxy Authentication Required',
    Q§The client must first authenticate itself with the proxy.§, :RFC<7235>;

    HTTP::Status.new: 408, 'Request Timeout',
    Q§The server timed out waiting for the request. According to HTTP specifications: "The client did not produce a request within the time that the server was prepared to wait. The client MAY repeat the request without modifications at any later time".§;

    HTTP::Status.new: 409, 'Conflict',
    Q§Indicates that the request could not be processed because of conflict in the current state of the resource, such as an edit conflict between multiple simultaneous updates.§;

    HTTP::Status.new: 410, 'Gone',
    Q§Indicates that the resource requested is no longer available and will not be available again. This should be used when a resource has been intentionally removed and the resource should be purged. Upon receiving a 410 status code, the client should not request the resource in the future. Clients such as search engines should remove the resource from their indices. Most use cases do not require clients and search engines to purge the resource, and a "404 Not Found" may be used instead.§;

    HTTP::Status.new: 411, 'Length Required',
    Q§The request did not specify the length of its content, which is required by the requested resource.§;

    HTTP::Status.new: 412, 'Precondition Failed',
    Q§The server does not meet one of the preconditions that the requester put on the request header fields.§, :RFC<7232>;

    HTTP::Status.new: 413, 'Payload Too Large',
    Q§The request is larger than the server is willing or able to process. Previously called "Request Entity Too Large".§, :RFC<7231>;

    HTTP::Status.new: 414, 'URI Too Long',
    Q§The URI provided was too long for the server to process. Often the result of too much data being encoded as a query-string of a GET request, in which case it should be converted to a POST request. Called "Request-URI Too Long" previously.§, :RFC<7231>;

    HTTP::Status.new: 415, 'Unsupported Media Type',
    Q§The request entity has a media type which the server or resource does not support. For example, the client uploads an image as image/svg+xml, but the server requires that images use a different format.§, :RFC<7231>;

    HTTP::Status.new: 416, 'Range Not Satisfiable',
    Q§The client has asked for a portion of the file (byte serving), but the server cannot supply that portion. For example, if the client asked for a part of the file that lies beyond the end of the file. Called "Requested Range Not Satisfiable" previously.§, :RFC<7233>;

    HTTP::Status.new: 417, 'Expectation Failed',
    Q§The server cannot meet the requirements of the Expect request-header field.§;

    HTTP::Status.new: 418, "I'm a teapot",
    Q§This code was defined in 1998 as one of the traditional IETF April Fools' jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol, and is not expected to be implemented by actual HTTP servers. The RFC specifies this code should be returned by teapots requested to brew coffee. This HTTP status is used as an Easter egg in some websites, such as Google.com's I'm a teapot easter egg.§, :RFC<7168>;

    HTTP::Status.new: 419, 'Page Expired',
    Q§Used by the Laravel Framework when a CSRF Token is missing or expired.§, :origin('Laravel Framework');

    HTTP::Status.new: 420, 'Enhance Your Calm',
    Q§Returned by version 1 of the Twitter Search and Trends API when the client is being rate limited; versions 1.1 and later use the 429 Too Many Requests response code instead. The phrase "Enhance your calm" comes from the 1993 movie Demolition Man, and its association with this number is likely a reference to cannabis.§, :origin<Twitter>;

    HTTP::Status.new: 421, 'Misdirected Request',
    Q§The request was directed at a server that is not able to produce a response[54] (for example because of connection reuse).§, :RFC<7540>;

    HTTP::Status.new: 422, 'Unprocessable Entity (WebDAV)',
    Q§The request was well-formed but was unable to be followed due to semantic errors.§, :RFC<4918>;

    HTTP::Status.new: 423, 'Locked (WebDAV)',
    Q§The resource that is being accessed is locked.§, :RFC<4918>;

    HTTP::Status.new: 424, 'Failed Dependency (WebDAV)',
    Q§The request failed because it depended on another request and that request failed (e.g., a PROPPATCH).§, :RFC<4918>;

    HTTP::Status.new: 425, 'Too Early',
    Q§Indicates that the server is unwilling to risk processing a request that might be replayed.§, :RFC<8470>;

    HTTP::Status.new: 426, 'Upgrade Required',
    Q§The client should switch to a different protocol such as TLS/1.3, given in the Upgrade header field.§;

    HTTP::Status.new: 428, 'Precondition Required',
    Q§The origin server requires the request to be conditional. Intended to prevent the 'lost update' problem, where a client GETs a resource's state, modifies it, and PUTs it back to the server, when meanwhile a third party has modified the state on the server, leading to a conflict.§, :RFC<6585>;

    HTTP::Status.new: 429, 'Too Many Requests',
    Q§The user has sent too many requests in a given amount of time. Intended for use with rate-limiting schemes.§, :RFC<6585>;

    HTTP::Status.new: 430, 'Request Header Fields Too Large',
    Q§Used by Shopify, instead of the 429 Too Many Requests response code, when too many URLs are requested within a certain time frame.§, :origin<Shopify>;

    HTTP::Status.new: 431, 'Request Header Fields Too Large',
    Q§The server is unwilling to process the request because either an individual header field, or all the header fields collectively, are too large.§, :RFC<6585>;

    HTTP::Status.new: 440, 'Login Time-out',
    Q§The client's session has expired and must log in again.§, :origin<IIS>;

    HTTP::Status.new: 444, 'No Response',
    Q§Used internally to instruct the server to return no information to the client and close the connection immediately.§, :origin<nginx>;

    HTTP::Status.new: 449, 'Retry With',
    Q§The server cannot honour the request because the user has not provided the required information.§, :origin<IIS>;

    HTTP::Status.new: 450, 'Blocked by Windows Parental Controls',
    Q§The Microsoft extension code indicated when Windows Parental Controls are turned on and are blocking access to the requested webpage.§, :origin<Microsoft>;

    HTTP::Status.new: 451, 'Unavailable For Legal Reasons',
    Q§A server operator has received a legal demand to deny access to a resource or to a set of resources that includes the requested resource. The code 451 was chosen as a reference to the novel Fahrenheit 451 (see the Acknowledgements in the RFC).§, :RFC<7725>;

    HTTP::Status.new: 460, 'Client Closed Connection with Load Balancer',
    Q§Client closed the connection with the load balancer before the idle timeout period elapsed. Typically when client timeout is sooner than the Elastic Load Balancer's timeout.§, :origin('AWS Elastic Load Balancer');

    HTTP::Status.new: 463, 'Too Many IPs in X-Forwarded-For',
    Q§The load balancer received an X-Forwarded-For request header with more than 30 IP addresses.§, :origin('AWS Elastic Load Balancer');

    HTTP::Status.new: 494, 'Request Header Too Large',
    Q§Client sent too large request or too long header line.§, :origin<nginx>;

    HTTP::Status.new: 495, 'SSL Certificate Error',
    Q§An expansion of the 400 Bad Request response code, used when the client has provided an invalid client certificate.§, :origin<nginx>;

    HTTP::Status.new: 496, 'SSL Certificate Required',
    Q§An expansion of the 400 Bad Request response code, used when a client certificate is required but not provided.§, :origin<nginx>;

    HTTP::Status.new: 497, 'HTTP Request Sent to HTTPS Port',
    Q§An expansion of the 400 Bad Request response code, used when the client has made a HTTP request to a port listening for HTTPS requests.§, :origin<nginx>;

    HTTP::Status.new: 498, 'Invalid Token',
    Q§Returned by ArcGIS for Server. Code 498 indicates an expired or otherwise invalid token.§, :origin<Esri>;

    HTTP::Status.new: 499, 'Client Closed Request',
    Q§Used when the client has closed the request before the server could send a response.§, :origin<nginx>;

    HTTP::Status.new: 500, 'Internal Server Error',
    Q§A generic error message, given when an unexpected condition was encountered and no more specific message is suitable.§;

    HTTP::Status.new: 501, 'Not Implemented',
    Q§The server either does not recognize the request method, or it lacks the ability to fulfil the request. Usually this implies future availability (e.g., a new feature of a web-service API).§;

    HTTP::Status.new: 502, 'Bad Gateway',
    Q§The server was acting as a gateway or proxy and received an invalid response from the upstream server.§;

    HTTP::Status.new: 503, 'Service Unavailable',
    Q§The server cannot handle the request (because it is overloaded or down for maintenance). Generally, this is a temporary state.§;

    HTTP::Status.new: 504, 'Gateway Timeout',
    Q§The server was acting as a gateway or proxy and did not receive a timely response from the upstream server.§;

    HTTP::Status.new: 505, 'HTTP Version Not Supported',
    Q§The server does not support the HTTP protocol version used in the request.§;

    HTTP::Status.new: 506, 'Variant Also Negotiates',
    Q§Transparent content negotiation for the request results in a circular reference.§, :RFC<2295>;

    HTTP::Status.new: 507, 'Insufficient Storage (WebDAV)',
    Q§The server is unable to store the representation needed to complete the request.§, :RFC<4918>;

    HTTP::Status.new: 508, 'Loop Detected (WebDAV)',
    Q§The server detected an infinite loop while processing the request (sent instead of 208 Already Reported).§, :RFC<5842>;

    HTTP::Status.new: 509, 'Bandwidth Limit Exceeded',
    Q§The server has exceeded the bandwidth specified by the server administrator; this is often used by shared hosting providers to limit the bandwidth of customers.§, :origin<cPanel>;

    HTTP::Status.new: 510, 'Not Extended',
    Q§Further extensions to the request are required for the server to fulfil it.§, :RFC<2774>;

    HTTP::Status.new: 511, 'Network Authentication Required',
    Q§The client needs to authenticate to gain network access. Intended for use by intercepting proxies used to control access to the network (e.g., "captive portals" used to require agreement to Terms of Service before granting full Internet access via a Wi-Fi hotspot).§, :RFC<6585>;

    HTTP::Status.new: 520, 'Web Server Returned an Unknown Error',
    Q§The origin server returned an empty, unknown, or unexpected response to Cloudflare.§, :origin<Cloudflare>;

    HTTP::Status.new: 521, 'Web Server is Down',
    Q§The origin server refused connections from Cloudflare. Security solutions at the origin may be blocking legitimate connections from certain Cloudflare IP addresses§, :origin<Cloudflare>;

    HTTP::Status.new: 522, 'Connection Timed Out',
    Q§Cloudflare timed out contacting the origin server.§, :origin<Cloudflare>;

    HTTP::Status.new: 523, 'Origin Is Unreachable',
    Q§Cloudflare could not reach the origin server; for example, if the DNS records for the origin server are incorrect or missing.§, :origin<Cloudflare>;

    HTTP::Status.new: 524, 'A Timeout Occurred',
    Q§Cloudflare was able to complete a TCP connection to the origin server, but did not receive a timely HTTP response.§, :origin<Cloudflare>;

    HTTP::Status.new: 525, 'SSL Handshake Failed',
    Q§Cloudflare could not negotiate a SSL/TLS handshake with the origin server.§, :origin<Cloudflare>;

    HTTP::Status.new: 526, 'Invalid SSL Certificate',
    Q§Cloudflare could not validate the SSL certificate on the origin web server. Also used by Cloud Foundry's gorouter.§, :origin<Cloudflare>;

    HTTP::Status.new: 527, 'Railgun Error',
    Q§Error 527 indicates an interrupted connection between Cloudflare and the origin server's Railgun server.§, :origin<Cloudflare>;

    HTTP::Status.new: 529, 'Site is overloaded',
    Q§Used by Qualys in the SSLLabs server testing API to signal that the site can't process the request.§, :origin<Qualys>;

    HTTP::Status.new: 530, 'Site is frozen',
    Q§Used by the Pantheon web platform to indicate a site that has been frozen due to inactivity.§, :origin<Pantheon>;

    HTTP::Status.new: 561, 'Unauthorized',
    Q§An error around authentication returned by a server registered with a load balancer. You configured a listener rule to authenticate users, but the identity provider (IdP) returned an error code when authenticating the user.§, :origin('AWS Elastic Load Balancer');

    HTTP::Status.new: 598, 'Network Read Timeout Error',
    Q§Used by some HTTP proxies to signal a network read timeout behind the proxy to a client in front of the proxy.§, :origin('(informal convention)');

    HTTP::Status.new: 599, 'Network Connect Timeout Error',
    Q§An error used by some HTTP proxies to signal a network connect timeout behind the proxy to a client in front of the proxy.§, :origin('(informal convention)');

    42  # make sure the previous HTTP::Status.new sinks
}

# legacy interface
our sub get_http_status_msg(Int() $code) is export {
    (@codes[$code] // 'Unknown').Str
}

my sub is-info(        $code) is export { 100 <= $code < 200 }
my sub is-success(     $code) is export { 200 <= $code < 300 }
my sub is-redirect(    $code) is export { 300 <= $code < 400 }
my sub is-error(       $code) is export { 400 <= $code < 600 }
my sub is-client-error($code) is export { 400 <= $code < 500 }
my sub is-server-error($code) is export { 500 <= $code < 600 }

# vim: expandtab shiftwidth=4
