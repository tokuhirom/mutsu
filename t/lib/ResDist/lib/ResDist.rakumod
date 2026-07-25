unit class ResDist;

# Pulls in a module from ANOTHER distribution, the way IO::Socket::SSL pulls in
# OpenSSL (whose OpenSSL::NativeLib reads `%?RESOURCES` at BEGIN time).
use ResInner;

method greeting() { ResInner.greeting }
