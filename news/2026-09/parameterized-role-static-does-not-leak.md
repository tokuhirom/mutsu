# Parameterized role statics no longer overwrite caller lexicals

Methods composed from a parameterized role can keep mutable `my` statics in the
role's class-body lexical store. Their writes are now kept out of the method
return merge, so a caller lexical with the same name is not rebound. This fixes
the `URI::FetchFile` `HTTP::UserAgent` provider path.

The canonical sandbox measurement now reaches both optional-provider test files
instead of dying in `HTTP::UserAgent.is-available`. Their remaining network
assertions are the same bundled-optional-module versus disabled-network
measurement gap tracked by mutsu issue #8844; the distribution is therefore
partial rather than green until the harness grows its network-test guard.
