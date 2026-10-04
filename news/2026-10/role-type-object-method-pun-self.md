Calling a role method on a role type object now gives the method the role's
composed pun as its dispatch context. A method that calls another method
provided by a composed role sees that method, while a direct call on the bare
role type object retains its usual behavior.
