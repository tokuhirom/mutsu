# A method call's samewith context no longer rescans the class or copies the arguments twice

Once a program names `samewith` or a deferral builtin, every method call
pushes a samewith context. Each push asked whether the receiver class is a user
HOW, a subclass of a built-in metamodel class whose `callsame` must reach the
native metamethod. Answering meant a registry lookup by name and a scan of the
class's MRO and parents. The push then copied the call's arguments, and the
pending deferral entry for the same call copied them a second time.

The user-HOW answer is now memoized per class for one registry generation. The
samewith context and the deferral entry also share a single `Arc<[Value]>` copy
of the arguments. On the AttrX::Mooish t/090-concurrent probe a round went from
4,954,262 to 4,926,157 instructions (callgrind slope R=120→360, -0.57%).
