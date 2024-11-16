// NOTICE: This file is a translation of an equivalent file in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit

open System

type TestException(message: string, inner: Exception) =
    inherit Exception(message, inner)

    new(message) = TestException(message, null)
    new() = TestException(null, null)
