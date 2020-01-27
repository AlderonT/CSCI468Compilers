.assembly extern mscorlib {}
.assembly Hello {}
.module Hello.exe
 
.method static void entrypoint()
cil managed
{
    .entrypoint
    ldstr "Bob {0}"
    ldc.i4 42
    box valuetype [mscorlib]System.Int32
    call void [mscorlib]System.Console::WriteLine(string,object)
    //call void [mscorlib]System.Console::WriteLine(string)
    ret
}