# IllogicApps

Portmanteau of "Illogical" and "Logic Apps", originally because it was probably never going to be able to emulate the logic in quite the same way, but also now because upon testing logic apps workflow edge cases, many of them make little to no sense and it has taken efforts to reproduce those surprising behaviours in this code.

This is intended to be a library used as a test harness for Logic Apps, similar to e.g. [LogicAppUnit] but mocked out so hard that we don't even have a real test host any more.
This is because the logic app test host is frustratingly slow, which means writing unit tests is a balancing act between covering important test cases and not making the overall unit test suite more painful to run for everyone involved.
Hopefully this can take the unit test runtimes of workflows down from 10s of seconds to 10s of milliseconds, or less if we're really lucky?

Currently in early development with no set-in-stone API, no support for most action types, many important expression functions not implemented, and many both minor and major discrepancies in behaviour between this and the real logic app test host.

[LogicAppUnit]: https://github.com/LogicAppUnit/TestingFramework
