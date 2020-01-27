.assembly extern mscorlib {}
.assembly Hello {}
.module Hello.exe
.method static void entrypoint()
cil managed
{
    .entrypoint
    .locals init (int32 v)
    ldc.i4 23
    ldc.i4 1
    add
    ldc.i4 3
    add
    stloc.s v
    ldstr "{0}"
    ldloc.s v
    box valuetype [mscorlib]System.Int32
    call void [mscorlib]System.Console::WriteLine(string,object)
    ret
}