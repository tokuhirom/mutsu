# Socket bind failures read the way rakudo reports them

`IO::Socket::Async.listen` quit with Rust's text ("Failed to bind: Cannot
assign requested address (os error 99)") and `IO::Socket::INET.new(:listen)`
died with "Failed to bind to '...': ...". Rakudo says `address not available
(address: 192.0.2.1:45678)` / `address already in use (address: ...)` /
`permission denied (address: ...)` for the Async listen and `Could not bind
socket: Address already in use` for INET, and programs match on those words.
HTTP::Core's `t/16-listen-failure` (`like $bad.bind-error, /'not available'/`)
now matches rakudo.
