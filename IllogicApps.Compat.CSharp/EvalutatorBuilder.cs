using IllogicApps.Core;
using IllogicApps.Expression.Execution;
using IllogicApps.Json;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Parser = IllogicApps.Expression.Parsing.Parser;

namespace IllogicApps.Compat.CSharp;

public class EvalutatorBuilder
{
    private OrderedMap.Builder<string, FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<JsonTree>, JsonTree>>>
        _functions = new();

    private OrderedMap.Builder<string, FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<Lazy<JsonTree>>, JsonTree>>>
        _lazyFunctions = new();

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
        Func<SimulatorContext, FSharpList<JsonTree>, JsonTree> functionToAdd)
    {
        _functions.Set(name, FuncConvert.FromFunc(functionToAdd));
        return this;
    }

    public EvalutatorBuilder SetLazyFunction(string name,
        Func<SimulatorContext, FSharpList<Lazy<JsonTree>>, JsonTree> functionToAdd)
    {
        _lazyFunctions.Set(name, FuncConvert.FromFunc(functionToAdd));
        return this;
    }

    class BuiltResult(
        OrderedMap<string, FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<JsonTree>, JsonTree>>> functions,
        OrderedMap<string, FSharpFunc<SimulatorContext, FSharpFunc<FSharpList<Lazy<JsonTree>>, JsonTree>>>
            lazyFunctions) : OptimizedClosures.FSharpFunc<SimulatorContext, Parser.Ast, JsonTree>
    {
        public override JsonTree Invoke(SimulatorContext simContext, Parser.Ast ast) =>
            Evaluator.evaluateSandboxed(functions, lazyFunctions, simContext, ast);
    }

    public FSharpFunc<SimulatorContext, FSharpFunc<Parser.Ast, JsonTree>> Build() =>
        new BuiltResult(_functions.Build(), _lazyFunctions.Build());
}