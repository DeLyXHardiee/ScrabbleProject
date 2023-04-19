// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
    type MultiSet<'a when 'a : comparison> = MultiSet of Map<'a, uint32>
    let empty = MultiSet Map.empty<'a, uint32>
    let isEmpty (MultiSet s) = Map.isEmpty s
    let size (MultiSet s) = Map.fold (fun state _ v -> state + v) 0u s
    let contains a (MultiSet s) = Map.containsKey a s
    let numItems a (MultiSet s) = 
        match Map.tryFind a s with
            | Some value -> value
            | None -> 0u
    let add a n (MultiSet s) = MultiSet (Map.add a n s)
    let addSingle a (MultiSet s) = MultiSet (Map.add a 1u s)
    let remove a n (MultiSet s) = 
        if Map.find a s <= n then
            MultiSet (Map.remove a s)
        else 
            MultiSet (Map.add a ((Map.find a s) - n) s)
    let removeSingle a (MultiSet s) = 
        if Map.containsKey a s then
            MultiSet (Map.add a ((Map.find a s) - 1u) s)
        else
            MultiSet s
    let fold f acc (MultiSet s) = Map.fold f acc s
    let foldBack f (MultiSet s) acc = Map.foldBack f s acc
    let ofList lst = List.fold(fun (s : MultiSet<'a>) x -> addSingle x s) empty lst
    let rec addToListRecursive (lst : 'a list) key (value : uint32) = 
        if value <= 0u then lst
        else addToListRecursive (key :: lst) key (value - 1u)
    let toList (MultiSet s) = Map.fold(fun (lst : 'a list) key value -> addToListRecursive lst key value) List.empty s
    (* let map f (MultiSet s) = Map.fold f empty s *)
    (* let union (MultiSet s1) (MultiSet s2) = 
        let result = Map.empty<'a, uint32>
        Map.iter(fun key value -> result.Add key value) s1 *)