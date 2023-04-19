// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    // 6.1

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))

    // 6.2

    let wordLength : SM<int> = 
        S (fun s -> Success ((s.word.Length), (s))) 

    // 6.3

    let characterValue (pos : int) : SM<char> = 
        S (fun s -> 
                match List.tryItem pos s.word with
                |Some x -> Success (fst x, s)
                |None -> Failure (IndexOutOfBounds pos))

    // 6.4

    let pointValue (pos : int) : SM<int> = 
        S (fun s -> 
                match List.tryItem pos s.word with
                |Some x -> Success (snd x, s)
                |None -> Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms
        S (fun s -> 
            match aux (s.vars) with
            | Some v -> Success (v, s)
            | None   -> Failure (VarNotFound x))

    // 6.6

    let declare (var : string) : SM<unit> = 
        let aux =
                function
                | []      -> []
                | m :: ms -> 
                    match Map.tryFind var m with
                    | Some v -> []
                    | None   -> Map.add var 0 m :: ms
        S (fun s -> 
            if s.reserved.Contains var then Failure (ReservedName var)
            else match aux (s.vars) with
                    | v -> Success ((), {s with vars = v})
                    | [] -> Failure (IndexOutOfBounds 0))

    // 6.5

    let update (var : string) (value : int) : SM<unit> = 
        let rec aux =
            function
            | []      -> []
            | m :: ms -> 
                match Map.tryFind var m with
                | Some v -> Map.add var value m :: ms
                | None   -> aux ms
        S (fun s -> 
            match aux (s.vars) with
            | v -> Success ((), {s with vars = v})
            | []    -> Failure (VarNotFound var))