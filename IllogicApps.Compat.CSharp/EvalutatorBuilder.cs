using IllogicApps.Core;
using IllogicApps.Expression.Execution;
using IllogicApps.Json;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Parser = IllogicApps.Expression.Parsing.Parser;

namespace IllogicApps.Compat.CSharp;

public class EvalutatorBuilder
{
    private OrderedMap.Builder<string,
            FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<CoreTypes.JsonTree>, CoreTypes.JsonTree>>>
        _functions = new();

    private OrderedMap.Builder<string, FSharpFunc<SimulatorContext,
        FSharpFunc<FSharpList<Lazy<CoreTypes.JsonTree>>, CoreTypes.JsonTree>>> _lazyFunctions = new();

    private EvalutatorBuilder()
    {
    }

    public static EvalutatorBuilder FromEmpty() => new();

    public static EvalutatorBuilder FromDefaults()
    {
        var evalutatorBuilder = new EvalutatorBuilder();
        evalutatorBuilder._functions.AddRange(BuiltinFunctions.functions);
        evalutatorBuilder._lazyFunctions.AddRange(BuiltinFunctions.lazyFunctions);
        return evalutatorBuilder;
    }

    public EvalutatorBuilder SetFunction(string name,
        Func<SimulatorContext, FSharpList<CoreTypes.JsonTree>, CoreTypes.JsonTree> functionToAdd)
    {
        _functions.Set(name, FuncConvert.FromFunc(functionToAdd));
        return this;
    }

    public EvalutatorBuilder SetLazyFunction(string name,
        Func<SimulatorContext, FSharpList<Lazy<CoreTypes.JsonTree>>, CoreTypes.JsonTree> functionToAdd)
    {
        _lazyFunctions.Set(name, FuncConvert.FromFunc(functionToAdd));
        return this;
    }

    class BuiltResult(
        OrderedMap<string, FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<CoreTypes.JsonTree>, CoreTypes.JsonTree>>>
            functions,
        OrderedMap<string, FSharpFunc<SimulatorContext,
            FSharpFunc<FSharpList<Lazy<CoreTypes.JsonTree>>, CoreTypes.JsonTree>>> lazyFunctions
    ) : OptimizedClosures.FSharpFunc<SimulatorContext, Parser.Ast, CoreTypes.JsonTree>
    {
        public override CoreTypes.JsonTree Invoke(SimulatorContext simContext, Parser.Ast ast) =>
            Evaluator.evaluateSandboxed(functions, lazyFunctions, simContext, ast);
    }

    public FSharpFunc<SimulatorContext, FSharpFunc<Parser.Ast, CoreTypes.JsonTree>> Build() =>
        new BuiltResult(_functions.Build(), _lazyFunctions.Build());
}