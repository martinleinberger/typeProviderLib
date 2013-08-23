module TutorialTypeProv

open System.Reflection
open Samples.FSharp.ProvidedTypes
open FSharp.Data.Json.Extensions

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Net
open System.IO
open FSharp.Data.Json

[<TypeProvider>]
type SampleTypeProvider(config: TypeProviderConfig) as this = class
    inherit TypeProviderForNamespaces()

    let namespaceName = "HelloWorldProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let makeOneProvidedType(n: int) =
        let t = ProvidedTypeDefinition(thisAssembly, namespaceName, "Type" + string n, baseType = Some typeof<obj>)

        t.AddXmlDocDelayed( fun() -> sprintf "This provided type %s" ("Type" + string n) )

        let staticProp = ProvidedProperty(propertyName = "StaticProperty",
                                          propertyType = typeof<string>,
                                          IsStatic = true,
                                          GetterCode = (fun args -> <@@ "Hello!" @@>))
                                          
        staticProp.AddXmlDocDelayed(fun () -> "This is a static property")
        
        t.AddMember staticProp 
        let ctor = ProvidedConstructor(parameters = [], InvokeCode = (fun args -> <@@ "The object data" :> obj @@>))
        ctor.AddXmlDocDelayed(fun () -> "This is a constructor")

        t.AddMember ctor

        let ctor2 = ProvidedConstructor(parameters = [ ProvidedParameter("data", typeof<string>) ], InvokeCode= (fun args -> <@@ (%%(args.[0]) : string) :> obj @@>))
        ctor2.AddXmlDocDelayed(fun () -> "This is a constructor")
        t.AddMember ctor2

        let instanceProp = ProvidedProperty(propertyName="InstanceProperty", propertyType = typeof<int>, GetterCode = (fun args -> <@@ ((%%(args.[0]) : obj) :?> string).Length @@>))
        instanceProp.AddXmlDocDelayed( fun() -> "This is an instance property" )
        t.AddMember instanceProp

        let instanceMeth = ProvidedMethod(methodName = "InstanceMethod", parameters = [ProvidedParameter("x", typeof<int>)], returnType = typeof<char>,
                                          InvokeCode = (fun args -> <@@ ((%%(args.[0]) : obj) :?> string).Chars(%%(args.[1]) : int) @@>))
        instanceMeth.AddXmlDocDelayed( fun() -> "This is an instance method" )
        t.AddMember instanceMeth

        t.AddMembersDelayed(fun () -> 
            let nestedType = ProvidedTypeDefinition("NestedType",
                                                    Some typeof<obj>)

            // Each nested type contains 100 static properties, provided on-demand.
            // The static properties have constant values.
            nestedType.AddMembersDelayed (fun () -> 
                let staticPropsInNestedType = 
                    [ for i in 1 .. 100 do
                        let valueOfTheProperty = "I am string "  + string i

                        let p = ProvidedProperty(propertyName = "StaticProperty" + string i, 
                                                 propertyType = typeof<string>, 
                                                 IsStatic=true,
                                                 GetterCode= (fun args -> <@@ valueOfTheProperty @@>))

                        p.AddXmlDocDelayed(fun () -> 
                               sprintf "This is StaticProperty%d on NestedType" i)

                        yield p ]
                
                staticPropsInNestedType)

            [nestedType])


        t

    let types = [ for i in 1 .. 10 -> makeOneProvidedType i ]

    do this.AddNamespace(namespaceName, types)
end