# A bracketed assignment infix in a hyper operator assigns

Brackets around a hyper operator's infix only group it, so `@a >>[+=]>> 1`
is `@a >>+=>> 1`. mutsu computed the sums but did not store them, because its
check for an assignment infix looked at the bracketed spelling. The compiler
now looks inside the brackets first.

CoreHackers::NfaChainsaw's `Utils::UserInterface` toggles its flags with
`@toggles >>[xor=]>> True`.
