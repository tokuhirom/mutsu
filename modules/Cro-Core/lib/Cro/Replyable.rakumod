use Cro::Transform;
use Cro::Sink;

# Something that does the `Cro::Replyable` role implements the `replier`
# method, which turns a `Cro::Sink` or `Cro::Transform` that should be used
# in order to handle the replies.
subset Cro::Replier where Cro::Sink | Cro::Transform;
role Cro::Replyable {
    method replier() returns Cro::Replier { ... }
}
