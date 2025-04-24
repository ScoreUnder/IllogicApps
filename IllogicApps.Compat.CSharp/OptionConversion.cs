using System.Runtime.CompilerServices;
using Microsoft.FSharp.Core;

namespace IllogicApps.Compat.CSharp;

public static class OptionConversion
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T? ToNullable<T>(this FSharpOption<T> option) =>
        FSharpOption<T>.get_IsNone(option) ? default : option.Value;
}