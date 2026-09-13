# Lexical pseudo-stashes respect their selected scope

`MY::` now lists only the current lexical scope, while `OUTER::MY::` resolves
symbols from the immediate outer scope. Interpolating `<<...>>` subscripts also
work on the composed pseudo-stash form.

Pinned by `t/modules/outer-my-pseudo-package.t`.

Closes #8213.
