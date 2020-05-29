namespace FSharpOSIsoft

//open PBObjLib
//open PBSymLib
open System.Text.RegularExpressions
//open System.Collections
open System.Runtime.CompilerServices
//open ExtensionMethods



//[<Extension>]
//type IEnumerableExtensions =
//    [<Extension>]
//    static member inline Sum(xs: 'T seq) = Seq.sum xs

//type IItem =
//    abstract Count: int 


//type IEnumerablePlus =
//    interface IEnumerable

//[<Extension>]
//type NonGenericIEnumerableExtensions =
//    [<Extension>]
//    static member inline enumerate(n:System.Collections.IEnumerable) =
//        let anItem = n.Item(0)
//        seq {
//            for (e: ^E) in n -> e
//        }

type PBDisplay = PBObjLib.Display
type PBSymbol = PBObjLib.Symbol
type PBText = PBSymLib.Text
type PBTrend = PBSymLib.Trend
type PBValue = PBSymLib.Value
type PBMultiState = PBObjLib.MultiState
type PBComposite = PBObjLib.Composite

type PITag = string
type PIServer = string
type PIPath = PIServer * PITag
type DataSet =  { name:string; expression: string; tags: PIPath list}
type DataItem =
    | TagDataItem of PIPath
    | DataSetItem of DataSet



type Class1() = 
    member this.X = "F#"

type TypedSymbol =
    | Text of PBText
    | Trend of PBTrend
    | Value of PBValue
    | MultiState of PBMultiState * TypedSymbol
    | Composite of PBComposite * TypedSymbol list
    | Untyped of PBSymbol

type IdentifiedSymbol = IdentifiedSymbol of name:string * TypedSymbol

type Display = Display of name:string * TypedSymbol list * DataSet list

type ProcBook = ProcBook of  name:string * Display list

type DisplayInProcbook = DisplayInProcbook of ProcBook * Display

type TagInDataSet = TagInDataSet of DataSet * PIPath

type PBFile =
    | PIW of path:string * ProcBook
    | PDI of path:string * Display

type TagOccurence =
    | TagOnSymbol of PBFile * Display * IdentifiedSymbol * DataItem
    | TagInDataSet of PBFile * Display * IdentifiedSymbol * DataSet * PIPath

module Helpers =
//    type TAsEnumerable = TAsEnumerable with
//        static member asEnumerable (TAsEnumerable, c:MatchCollection) = ExtensionMethods.AsEnumerable c
//        static member asEnumerable (TAsEnumerable, c:GroupCollection) = ExtensionMethods.AsEnumerable c
//        static member asEnumerable (TAsEnumerable, c:PBObjLib.Displays) = ExtensionMethods.AsEnumerable c

    //let inline asEnumerable c = ((^C or ^a) : (static member asEnumerable : ^C * ^a -> _) (TAsEnumerable, c))
    //type EnumerableClass() =
    //    static member inline enumerate (c: ^C when ^C :> System.Collections.IEnumerable and ^C : (member get_Item : int -> ^E)): seq< ^E> 
    //let inline enumerate< ^C, ^D, ^E when  (^C or ^D) : (static member get_Item : int -> ^E)> (c: ^C): seq< ^E> =
    //    let tryItem (c: ^C) = c.get_Item(0)
    //    seq {
    //        for e in c -> e
    //    }
    
module Seq =
    //let inline inferCast s = 
        // constrain ^t to have an Item indexed property (which we don't actually invoke)
        //let _a = fun x -> (^r : (member Item : int -> ^v with get) (x, 0))
        //let _b = fun y -> (^s : (member Item : int -> ^v) (y, 0))
        //let _ = fun z -> (^t :  (z, 0))


        //let _ = fun x -> (^t : (member Item : int -> ^v with get) (x, 0))
        //let e = (^t : (member GetEnumerator : unit -> ^e) s)
        //seq { while (^e : (member MoveNext : unit -> bool) e) do
        //        yield (^e : (member Current : obj) e) :?> ^v }

    
    let inline enumerate< ^t, ^v, ^e when ^t : (member Item : int -> ^v with get) and ^t : (member GetEnumerator : unit -> ^e) and ^e : (member MoveNext : unit -> bool) and ^e : (member Current : obj)> s = 
        // constrain ^t to have an Item indexed property (which we don't actually invoke)
        //let _a = fun x -> (^r : (member Item : int -> ^v with get) (x, 0))
        //let _b = fun y -> (^s : (member Item : int -> ^v) (y, 0))
        //let _ = fun z -> (^t :  (z, 0))
        let _ = fun x -> (^t : (member Item : int -> ^v with get) (x, 0))
        let e = (^t : (member GetEnumerator : unit -> ^e) s)
        seq { while (^e : (member MoveNext : unit -> bool) e) do
                yield (^e : (member Current : obj) e) :?> ^v }

    let inline enumerateB< ^t, ^v, ^e when ^t : (member Item : int -> ^v) and ^t : (member GetEnumerator : unit -> ^e) and ^e : (member MoveNext : unit -> bool) and ^e : (member Current : obj)> s = 
          // constrain ^t to have an Item indexed property (which we don't actually invoke)
          //let _a = fun x -> (^r : (member Item : int -> ^v with get) (x, 0))
          //let _b = fun y -> (^s : (member Item : int -> ^v) (y, 0))
          //let _ = fun z -> (^t :  (z, 0))
          let _ = fun x -> (^t : (member Item : int -> ^v) (x, 0))
          let e = (^t : (member GetEnumerator : unit -> ^e) s)
          seq { while (^e : (member MoveNext : unit -> bool) e) do
                  yield (^e : (member Current : obj) e) :?> ^v }

    //let inline inferCast2 s = 
    //    // constrain ^t to have an Item indexed property (which we don't actually invoke)
    //    //let _a = fun x -> (^r : (member Item : int -> ^v with get) (x, 0))
    //    //let _b = fun y -> (^s : (member Item : int -> ^v) (y, 0))
    //    //let _ = fun z -> (^t :  (z, 0))
    //    let _ = fun x -> (^t : (member Item : int -> ^v) (x, 0))
    //    let e = (^t : (member GetEnumerator : unit -> ^e) s)
    //    seq { while (^e : (member MoveNext : unit -> bool) e) do
    //            yield (^e : (member Current : obj) e) :?> ^v }

//type HasAsEnumerable =
//    static inline member AsEnumerable ()
//type IAsEnumerable =
//    abstract member AsEnumerable: unit -> Generic.IEnumerable< ^E>

module ProcBook =
    //open Helpers

    //let inline testHG (ms:MatchCollection): IAsEnumerable=
    //    ms :> IAsEnumerable

    //let testF (ms:(MatchCollection and IEnumerable)) =
    //    let nms = ExtensionMethods.AsEnumerable ms
    //    nms
    //let inline enumerate (coll: ^C when ^C :> System.Collections.IEnumerable and ^C : (member get_Item : int -> ^E)): seq< ^E> = seq { for (e:^E) in coll -> e }
    //let inline enumerateB
    //    (coll: ^C when ^C :> IEnumerable and ^C : (member Item:^I -> ^E)): seq< ^E> =
    //        Seq.cast coll

    let inline konst x _ = x

    type CFunctor() =
        static member inline fmap (f: ^a -> ^b, a: System.Collections.Generic.IEnumerable< ^a>) = Seq.map f a
        static member inline fmap (f: ^a -> ^b, a: ^a list) = List.map f a
        static member inline fmap (f: ^a -> ^b, a: ^a option) =
            match a with
            | None -> None
            | Some x -> Some (f x)

        // default implementation of replace
        static member inline replace< ^a, ^b, ^c, ^d, ^e when ^a :> CFunctor and (^a or ^d): (static member fmap: (^b -> ^c) * ^d -> ^e) > (a, f) =
            ((^a or ^d) : (static member fmap : (^b -> ^c) * ^d -> ^e) (konst a, f))

        // call overridden replace if present
        static member inline replace< ^a, ^b, ^c when ^b: (static member replace: ^a * ^b -> ^c)>(a: ^a, f: ^b) =
            (^b : (static member replace: ^a * ^b -> ^c) (a, f))

    let inline replace_instance< ^a, ^b, ^c, ^d when (^a or ^c): (static member replace: ^b * ^c -> ^d)> (a: ^b, f: ^c) =
            ((^a or ^c): (static member replace: ^b * ^c -> ^d) (a, f))

    // Note the concrete type 'CFunctor' specified in the signature
    let inline replace (a: ^a) (f: ^b): ^a0 when (CFunctor or  ^b): (static member replace: ^a *  ^b ->  ^a0) =
        replace_instance<CFunctor, _, _, _> (a, f)


    
    type VersEnum() =
        static member inline anItem< ^c, ^e, ^i, ^d when ^c: ( static member Item: ^i * ^c -> ^e)> (coll: ^c) =
            fun (i: ^i) -> (^c : (static member Item : ^i * ^c -> ^e) (i, coll))
        static member inline 


    let mapTest (a: string list) =
        CFunctor.fmap(String.length,a)
    //let inline enumerated (coll: ^C when ^C :> System.Collections.IEnumerable and ^C : (static member AsEnumerable : unit -> Generic.IEnumerable< ^E>)): Generic.IEnumerable< ^E>  =
    // C - the particular collection
    // N - the collection casted to I
    // I - the interface with AsEnumerable
    // E - the particular element type
    //let inline onHas (h:HasAsEnumerable< ^T>) =
    //    h.AsEnumerable()

    ////let inline enumerateC< ^C, ^I, ^N, ^E when ^C :> ^I and ^N :> Generic.IEnumerable< ^E> and (^I or ^C) : (static member AsEnumerable : ^C -> ^E)> (coll: ^C): ^N =
    //let inline enumerateC (coll: ^C when ^C :> HasAsEnumerable< ^T>) =
    //    coll.AsEnumerable

    //let (|Enumerated|) (coll:System.Collections.IEnumerable) = enumerate coll
    let PITagRegEx = @"([A-Za-z0-9_-]\.)*[A-Za-z0-9_-]"
    let PIPathRegExString = @"\\\\([^\\]+)\\(([A-Za-z0-9_-]*\.)*[A-Za-z0-9_-]*)"
    let PDIFilePathRegExString = @".+(p|P)(d|D)(i|I)"
    let PIWFilePathRegExString = @".+(p|P)(i|I)(w|W)"

    let PIPathRegEx = new Regex(PIPathRegExString)
    let PDIFilePathRegEx = new Regex(PDIFilePathRegExString)
    let PIWFilePathRegEx = new Regex(PIPathRegExString)

    let (|PIWPath|PDIPath|InvalidPath|) (path:string) =
        match PIWFilePathRegEx.IsMatch(path) with
        | true -> PIWPath path
        | false ->
            match PDIFilePathRegEx.IsMatch(path) with
            | true -> PDIPath path
            | false -> InvalidPath
            

    let matches = PIPathRegEx.Matches @"\\EDS_PI\ERCOT.NEDIN.UN.GT1.ANLG.MW"
    let enumeratedPre (ms:MatchCollection) = seq { for m in ms -> m}
    let mcTest (ms:MatchCollection) =
        let possibleFirst = seq {for i in ms -> i}
        possibleFirst

    //let inline
    let dsTest (ms:PBObjLib.DisplaysClass) = Seq.enumerateB ms
    
    let ssTest (ms:PBObjLib.SymbolsClass) = Seq.inferCast2 ms

    //let inline inlineTest (coll:^T when ^T : (member GetEnumerator: ^I -> ^C)): ^C =
    //    //let sq = seq { for i in coll -> i}
    //    //sq
    //    let tmp = coll.
    //    let nm = tmp.

    //let mcTested (ms:MatchCollection) = inlineTest ms

    //let inline enumerated (ms:MatchCollection) = 
    //    //let t = ms
    //    //let unused = fun x -> (^t : (static member AsEnumerable : ^t -> Generic.IEnumerable< ^E>) x)
    //    //let tst (n: ^t) = unused n
    //    let t = ms.A :> HasAsEnumerable<Match>
    //    let tst = enumerateC ms
    //    tst ms

    let firstMatch = matches.Item(0)
    let matchedServer = firstMatch.Groups.Item(1)
    let matchedPath = firstMatch.Groups.Item(2)

    let (|ValidPath|_|) (s:string) =
        //s
        let ms = PIPathRegEx.Matches s
        let ns = asEnumerable ms
        s
        |> PIPathRegEx.Matches
        |> asEnumerable
        |> Seq.toList
        //|> (PIPathRegEx.Matches >> asEnumerable >> Seq.toList)
        |> function
            | head :: tail ->
                head.Groups
                |> (Seq.inferCast >> Seq.toList)
                |> function
                    | fullmatch :: server :: tag :: _ -> Some (PIPath (server.Value,tag.Value))
                    | _ -> None
            | _ -> None

    //let (|ValidPathB|_|) (s:string) =
    //    let ms = PIPathRegEx.Matches s
    //    let nms = ms.AsEnumerable()
    //    s
    //    |> (PIPathRegEx.Matches >>  Seq.toList)
    //    |> function
    //        | head :: tail ->
    //            head.Groups
    //            |> (Seq.inferCast >> Seq.toList)
    //            |> function
    //                | fullmatch :: server :: tag :: _ -> Some (PIPath (server.Value,tag.Value))
    //                | _ -> None
    //        | _ -> None
        //seq { for m in matches -> m }
        //|> Seq.toList
        //|> function
        //    | Some m -> seq { for g in m.Groups -> g }

    let (|RGB|) (col : System.Drawing.Color) =
        ( col.R, col.G, col.B )

    let testOnCOl (c:System.Drawing.Color list) =
        let ts = seq { for (RGB (r,g,b)) in c -> r+g+b}
        ts


    //let (|TextSym|TrendSym|ValueSym|MultiStateSym|CompositeSym|UntypedSym|) (s:Symbol) =
    //    match s with 
    //    | :? Text as x -> TextSym (Text x)
    //    | :? Trend as t -> TrendSym (Trend t)
    //    | :? Value as v -> ValueSym (Value v)
    //    | :? MultiState as m -> MultiStateSym (MultiState m)
    //    | :? Composite as c -> CompositeSym (Composite c)
    //    | _             -> None

    let rec (|Typed|) (s:PBSymbol) =
        match s with 
        | :? PBText as x -> (Text x)
        | :? PBTrend as t -> (Trend t)
        | :? PBValue as v -> (Value v)
        | :? PBMultiState as m -> 
            m.Parent
            |> fun (Typed p) -> (MultiState (m,p))
            //m.Parent |> fun (Typed p) -> Typed (MultiState (m,p))
        | :? PBComposite as c ->
            seq{ for (Typed el) in c.GroupedSymbols -> el }
            |>Seq.toList
            |>(fun (e) -> Composite (c,e))
        | _ -> Untyped s

    let symsInDisplay (d:PBDisplay) =
        seq { for sym in d.Symbols -> sym }
        |> Seq.map (
            function
            | Typed s -> s
            )


    let enumEntries (es:PBObjLib.EntriesClass) = enumerate es
    let enumDisplays (es:PBObjLib.Displays) = asEnumerable es
    let enumSymbols (es:PBObjLib.SymbolsClass) = enumerate es

    let processFile (app:PBObjLib.Application) (filepath:string) =
        match filepath with
        | PDIPath p ->
            let disp = app.Displays.Open(p,true)
            Ok (symsInDisplay disp)
        | PIWPath p ->
            let piw = app.ProcBooks.Open(p)
            match piw with
            | :? PBObjLib.ProcBook as pb ->
                pb.Entries
                |> Seq.cast
                |> fun (s:PBObjLib.Entry seq) -> seq {for e in s -> e :?> PBObjLib.Display}
                |> Seq.map symsInDisplay
                |> Seq.concat
                |> Ok
            | _ -> Error "Opening piw file did not return a valid ProcBook object"

        | InvalidPath  -> Error "Not a valid ProcessBook file path."

    let activeWorkbook (app : PBObjLib.Application) =
        let disp = app.ActiveDisplay
        let syms = disp.Symbols
        let casted = Seq.cast<PBSymbol> syms
        let usingComp = seq { for s in syms -> s }
        let testOnSym (s:PBSymbol) =
            s.Selected
        let processedSyms =Seq.toList (symsInDisplay disp)

        disp
    //let decollectionalize (coll:System.Collections.IEnumerable) =
    //    match coll with
    //    | :? seq<typeof<coll.Item>> as s -> s
    //    //seq {for (el:'T) in coll -> el}

    //let firstSym =
    //    let symsType = syms.GetType()
    //    let enumer = syms.GetEnumerator()
    //    symsType
        




    //let maxhash = max << hash
    // The following is acceptable because the argument for maxhash is explicit:
    let maxhash2 obj = (max << hash) obj

    //let emptyList10 = Array.create 10 []
    // Adding an extra (unused) parameter makes it a function, which is generalizable.
    let emptyListB10 () = Array.create 10 []

    //let arrayOf10Lists = Array.create 10 []
    // Adding a type parameter and type annotation lets you write a generic value.
    let arrayOf10BLists<'T> = Array.create 10 ([]:'T list)

    let intLists = arrayOf10BLists<int>
    let floatLists = arrayOf10BLists<float>

