namespace HelloWorldProvider

open System.Reflection
open Samples.FSharp.ProvidedTypes
open FSharp.Data.Json.Extensions

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Net
open System.IO
open FSharp.Data.Json

[<AutoOpen>]
module HelperModule =
    let load url = 
        let req = WebRequest.Create(url : string)
        JsonValue.Load(req.GetResponse().GetResponseStream())

    let loadFromFs path =
        let reader = new StreamReader(path:string)
        JsonValue.Load(reader)

    type ExplorerData(data:JsonValue) as x = class
        let mutable containedObjects: Map<string, ExplorerData> = Map.empty
        let mutable stringArrays: Map<string, seq<string>> = Map.empty
        let mutable objectArrays: Map<string, seq<ExplorerData>> = Map.empty

        do
            let objects =
                data.Properties |> Seq.filter( fun (key, value) ->
                    //keine ahnung obs ne bessere Möglichkeit gibt zu checken ob value ein json object ist
                    try
                        value.Properties |> Seq.iter( fun args -> () )
                        true
                    with
                        _ -> false
                )
                |> Seq.map( fun (key, value) ->
                    let value' = ExplorerData(value)
                    key, value'
                )

            let strLists = 
                data.Properties |> Seq.filter( fun (key, value) ->
                    //test obs ein string array ist
                    try
                        value.Item(0).AsString() |> ignore
                        true    
                    with
                        _ -> false                 
                )
                |> Seq.map( fun (key, value) ->
                    let value' = value.AsArray() |> Seq.map( fun x -> x.AsString() )
                    key, value'
                )

            let objLists = 
                data.Properties |> Seq.filter ( fun (key, value) ->
                    try
                        value.Item(0).Properties |> ignore
                        true
                    with
                        _ -> false
                )
                |> Seq.map( fun (key, value) ->
                    let value' = value.AsArray() |> Seq.map( fun x -> new ExplorerData(x) )
                    key, value'
                )
            
            x.ContainedObjects <- Map.ofSeq objects
            x.StringArrays <- Map.ofSeq strLists
            x.ObjectArrays <- Map.ofSeq objLists

        new (url:string) =
            let d = load url
            ExplorerData(d)

        member private x.ContainedObjects
            with get() = containedObjects
            and set(v) = containedObjects <- v

        member private x.StringArrays
            with get() = stringArrays
            and set(v) = stringArrays <- v

        member private x.ObjectArrays
            with get() = objectArrays
            and set(v) = objectArrays <- v

        member x.Data = data
        member x.GetString(propertyName:string) =
            x.Data.GetProperty(propertyName).AsString()
        
        member x.GetObject(pName:string) = 
            x.ContainedObjects.Item(pName)

        member x.GetStringArray(pName:string) = 
            x.StringArrays.Item(pName)

        member x.GetObjectArray(pName:string) = 
            x.ObjectArrays.Item(pName)
    end

    //---------- helper methods for property creation ----------\\
    let makeStringProperty(propertyName, xmlDoc:string) = 
        let p = ProvidedProperty(propertyName = propertyName, propertyType = typeof<string>, IsStatic = false, GetterCode = (fun [ me ] ->
            <@@ ((%%me:obj):?>ExplorerData).GetString(propertyName) @@>
        ))
        p.AddXmlDoc xmlDoc
        p

    let makeObjectProperty(propertyName, xmlDoc, typeDefinition) = 
        let p = ProvidedProperty(propertyName=propertyName, propertyType = typeDefinition, GetterCode = (fun [me] ->
            <@@ ((%%me:obj):?>ExplorerData).GetObject(propertyName) @@>
        ))
        p.AddXmlDoc xmlDoc       
        p

    let makeStringArrayProperty(propertyName, xmlDoc) = 
        let p = ProvidedProperty(propertyName=propertyName, propertyType = typedefof<seq<_>>.MakeGenericType(typeof<string>), GetterCode = ( fun [me] ->
            <@@  ((%%me:obj):?>ExplorerData).GetStringArray(propertyName) @@>
        ))
        p.AddXmlDoc xmlDoc
        p

    let makeObjectArrayProperty(propertyName, xmlDoc, typeDefinition:ProvidedTypeDefinition) =
        let p = ProvidedProperty(propertyName=propertyName, propertyType = typedefof<seq<_>>.MakeGenericType(typeDefinition), GetterCode = ( fun [me] ->
            <@@ ((%%me:obj):?>ExplorerData).GetObjectArray(propertyName) @@>
        ))
        p.AddXmlDoc xmlDoc
        p

    //----------   actual parsing ----------\\
    let rec parse(key:string, value:JsonValue, initialCall:bool) =
        let typeDefinition =
            if initialCall then
                ProvidedTypeDefinition(Assembly.GetExecutingAssembly(), "OneOhOne", key, baseType = Some typeof<obj>)
            else
                ProvidedTypeDefinition(key, baseType = Some typeof<obj>)

        value.GetProperty("properties").Properties |> Seq.iter( fun (key', value') ->
            let valType = value'.GetProperty("type").AsString()
            match valType with
                | "string" ->
                    let doc = "<summary>"+value'.GetProperty("description").AsString()+"</summary>"
                    let p = makeStringProperty(key', doc)
                    typeDefinition.AddMember p
                | "object" ->
                    //first add newly created type, then add actual property
                    let t' = parse(key', value', false)
                    typeDefinition.AddMember t'

                    let doc = "<summary>"+value'.GetProperty("description").AsString()+"</summary>"
                    let p = makeObjectProperty(key', doc, t') 
                    typeDefinition.AddMember p
                | "array" ->
                    let itemType = value'.GetProperty("items").GetProperty("type").AsString()
                    match itemType with
                        | "string" ->
                            let doc = "<summary>" + value'.GetProperty("description").AsString() + "</summary>"
                            let p = makeStringArrayProperty(key', doc)
                            typeDefinition.AddMember p
                        | "object" ->
                            let t' = parse(key'+"Item", value'.GetProperty("items"), false)
                            typeDefinition.AddMember t'
                            
                            let doc = "<summary>" + value'.GetProperty("description").AsString() + "</summary>"
                            let p = makeObjectArrayProperty(key', doc, t')
                            typeDefinition.AddMember p
                        | _ ->
                            let errMsg = "unkown type in array " + key'
                            failwith errMsg
                | _ ->
                    let errMsg = "unkown type in object " + key
                    failwith errMsg
        ) 

        typeDefinition:ProvidedTypeDefinition
        

[<TypeProvider>]
type OneOhOneProvider(config: TypeProviderConfig) as this = class
    inherit TypeProviderForNamespaces()

    let namespaceName = "OneOhOne"
    //let fileSchema = loadFromFs "C:/Users/Martin/Desktop/101repo_file.json"
    let fileSchema = load "https://raw.github.com/101companies/101worker/master/schemas/101repo_file.json"

    let makeFileType = 
        let t = parse("File", fileSchema, true)
        let ctor = ProvidedConstructor(
                    parameters = [ ProvidedParameter("targetUrl", parameterType=typeof<string>) ],
                    InvokeCode = (fun [ targetUrl ] -> <@@ new ExplorerData(%%targetUrl:string) @@>)
        )
        ctor.AddXmlDoc("<summary>Constructor for a file</summary>")
        t.AddMember ctor

        t

    let types = [ makeFileType ]

    do
        this.AddNamespace(namespaceName, types)

end

[<assembly:TypeProviderAssembly>]
do()