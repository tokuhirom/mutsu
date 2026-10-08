# The progress of a failed Seq pull no longer leaks into an enclosing Seq

The progress a failing `.map`/`.grep`/`Iterator` pull attaches to its exception is only meant for the Seq that was pulled. A consuming read, a sink or a prefix pull of an inner Seq inside a map callback left it on the exception, so the enclosing Seq's `pull_and_store` took it for its own and resumed at the wrong position. Those paths now drop the progress. Follow-up to #12394; the design that removes the side-channel is #12401.
