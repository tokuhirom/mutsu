# A cached Seq can be read from several threads at once

Reading one `.cache`d Seq that had not been reified yet from several
`start` blocks threw "The iterator of this Seq is already in use/consumed"
(#11242). The first reader moved the Seq's source out of the state lock to
pull it, and a second thread arriving meanwhile read the moved-out source as
consumed. A non-consuming pull now claims the body: other threads wait for
the pull to finish and then read what it stored. A pull that re-enters the
same Seq on its own thread still sees it as in use, as before.

The same window also let a reader race a writer. Each pull publishes a new
generation of elements, and readers used to find the newest one by looking
at the end of a `Vec` that the next publication could reallocate under them.
Generations now live in `Arc`s kept until the Seq is dropped, and readers
load the newest one through an atomic pointer, so they never touch the `Vec`
that a publication grows.
