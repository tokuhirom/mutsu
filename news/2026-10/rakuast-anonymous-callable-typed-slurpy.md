RakuAST now retains traits and defaults on anonymous callable signature
parameters such as `& is raw = &say`, and the type of scalar slurpies such as
`Code *$block`. The signature introspection and slurpy-blocks roast files pass
in RakuAST frontend mode.
