module Dictionary
    type Dictionary =
    | Node of Map<char, Dictionary> * bool // Dict with children Dicts containing characters and a boolean "end of the word"
    
    let empty () = Node(Map.empty, false)

    let insert (word : string) (dict : Dictionary) =
        let rec recursiveInsertion (word : string) (dict : Dictionary) (index : int) (updatedEndOfWord : bool) =
            match dict with
            | Node(children, endOfWord) ->
                if index = word.Length then
                    Node(children, true)
                else
                    let character = word.[index]
                    match Map.tryFind character children with
                    | Some(child) ->
                        let updatedChild = recursiveInsertion word child (index + 1) updatedEndOfWord
                        Node(Map.add character updatedChild children, endOfWord)
                    | None ->
                        let newChild = recursiveInsertion word (empty ()) (index + 1) updatedEndOfWord
                        Node(Map.add character newChild children, endOfWord)
        recursiveInsertion word dict 0 false
    
    let lookup (word : string) (dict : Dictionary) =
        let rec recursiveLooking (word : string) (dict : Dictionary) (index : int) =
            if index = word.Length - 1 then
                match dict with
                | Node(children, _) ->
                    match Map.tryFind word.[index] children with
                    | Some(child) ->
                        match child with
                        | Node(_, endOfWord) -> endOfWord
                    | None -> false
            else
                match dict with
                | Node(children, _) -> 
                    match Map.tryFind word.[index] children with
                    | Some(child) -> recursiveLooking word child (index + 1)
                    | None -> false
        recursiveLooking word dict 0