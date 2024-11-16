// NOTICE: This file is a translation of equivalent files in the LogicAppUnit project, and as such is licensed
// under the same terms as that project. The license can be found in this project's directory.
namespace LogicAppUnit.Wrapper

type CsxWrapper(script: string, relativePath: string, filename: string) =
    member _.Script = script
    member _.RelativePath = relativePath
    member _.Filename = filename
