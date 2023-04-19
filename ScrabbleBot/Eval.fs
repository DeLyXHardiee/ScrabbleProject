// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

(* Code for testing *)

    let emptyState = mkState [] [] []
    
    // 6.7

    let add (a : SM<int>) (b : SM<int>) = 
        a >>= (fun v1 -> b >>= (fun v2 -> ret(v1 + v2)))

    // 6.8

    let div (a : SM<int>) (b : SM<int>) = 
        b >>= (fun v -> match v with
                            |0 -> fail DivisionByZero
                            |_ -> a >>= (fun v1 -> b >>= (fun v2 -> ret(v1 / v2))))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    // 6.9

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV a -> arithEval a >>= (fun v -> pointValue v)
        | Add(a, b) -> 
            arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 + v2)))
        | Sub(a, b) ->
            arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 - v2)))
        | Mul(a, b) ->
            arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 * v2)))
        | Div(a, b) ->
            arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 / v2)))
        | Mod(a, b) ->
            arithEval b >>= fun v2 -> if v2 <> 0 then arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 % v2))) else fail DivisionByZero
        | CharToInt c -> charEval c >>= (fun v -> ret (int v))

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= (fun v -> characterValue v)
        | ToUpper c -> charEval c >>= (fun v -> ret (System.Char.ToUpper v))
        | ToLower c -> charEval c >>= (fun v -> ret (System.Char.ToLower v))
        | IntToChar a -> arithEval a >>= (fun v -> ret (char v))
    
    let isVowel (c : char) =
        if "aeiouyæøå".Contains(System.Char.ToLower c) then true
        else false

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq(a, b) -> arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 = v2)))
        | ALt(a, b) -> arithEval a >>= (fun v1 -> arithEval b >>= (fun v2 -> ret (v1 < v2)))
        | Not b -> boolEval b >>= (fun v -> ret (not v))
        | Conj(a, b) -> boolEval a >>= (fun v1 -> boolEval b >>= (fun v2 -> ret (v1 && v2)))
        | IsDigit c -> charEval c >>= (fun v -> ret (System.Char.IsDigit(v)))
        | IsLetter c -> charEval c >>= (fun v -> ret (System.Char.IsLetter(v)))
        | IsVowel c -> charEval c >>= (fun v -> ret (isVowel v))


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    