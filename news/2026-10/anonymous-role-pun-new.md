# Role declaration values can be instantiated

A `role` declaration used as an expression evaluates to its individual role candidate. Calling
`.new` on that value now normalizes it to the registered role group and follows the regular role
pun construction path. Anonymous and named role declaration values both construct instances with
their declared attribute defaults.
