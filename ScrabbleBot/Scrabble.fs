namespace NiceSpare

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO
open System.Text.RegularExpressions

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    //open Dict
    open Parser

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"            
            forcePrint "lul\n\n"

            let convertToPoints (value:uint32) (pieces:Map<uint32, tile>) : int = 
                let points = match Map.tryFind value pieces with
                                        | Some tile -> match Set.maxElement tile with
                                                            | piece -> snd piece
                                        | None -> failwith "Not a valid character"
                points

            let uintToChar id = char(id + 64u)
            let charToUint c = uint32(c)-64u
            let myhand = MultiSet.empty
            let handWithTiles = MultiSet.add 1u 1u myhand
            let handWithTiles = MultiSet.add 16u 1u handWithTiles
            let handWithTiles = MultiSet.add 5u 1u handWithTiles
            let handWithTiles = MultiSet.add 19u 1u handWithTiles
            
            // Call when you have found starter and propegate dictionary into findValidWord
            let rec stepIntoWord (word:string) (dict:Dictionary.Dict) : Dictionary.Dict =
                let characters = Seq.toList word
                let rec aux (c:char) (startDict:Dictionary.Dict)= 
                    match Dictionary.step c startDict with
                    | Some (_,subDict: Dictionary.Dict) -> aux c subDict 
                    | None -> failwith "shouldnt happen"
                List.fold (fun d c -> aux c d) dict characters
                    
            let rec findValidWord (hand:MultiSet.MultiSet<uint32>) (dict:Dictionary.Dict) (startPos:coord) (localAcc:string) : string =
                MultiSet.fold (fun acc letter count -> 
                    match Dictionary.step (uintToChar letter) dict with 
                    | Some (endOfWord, subDict) ->
                        let sLetter = (uintToChar letter)
                        let currentString = localAcc + (string) sLetter
                        let newHand = MultiSet.removeSingle letter hand
                        let branch = findValidWord newHand subDict startPos currentString
                        if endOfWord && currentString.Length > branch.Length && currentString.Length > acc.Length then currentString
                        elif branch.Length > acc.Length then branch
                        else acc 
                    | None -> acc
                ) "" hand
            let validWord = findValidWord st.hand st.dict (0,0) ("")
            
            
            forcePrint validWord
            forcePrint "validWord\n"

            let makeMove (startPos:coord) (direction:coord) (word:string)  = 
                let rec moveHelper (pos:coord) dir remainingWord (moves:list<coord * (uint32 * (char * int))>) =
                    let newPos:coord =
                        if Coord.getX dir = 1 then
                            Coord.mkCoordinate (Coord.getX pos + 1) (Coord.getY pos)
                        else
                            Coord.mkCoordinate (Coord.getX pos) (Coord.getY pos + 1)
                    match remainingWord with
                    | [] -> moves
                    | w::xs -> 
                        let move = (newPos,(charToUint (char(w)),(char(w),convertToPoints (charToUint(w)) pieces)))
                        moveHelper newPos dir xs (move :: moves)
                moveHelper startPos direction (Seq.toList(word)) []

            let move = makeMove (0,0) (0,1) (findValidWord st.hand st.dict (0,0) "") 
            //forcePrint moves
            //(move:list<(int * int) * (uint32 * (char * int))>)
            //list<(int * int) * (uint32 * (char * int))>

            //let findStarters() = 
            //stepIntoWord
            // findValidWord (stepIntoWord dict)
            // makeMove starter            


            let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input
            forcePrint "lul"
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                

            
                // step på bogstav
                //match some = en vej videre med bogstav

                // kald findvalidword rekursivt uden det brugte bogstav

                //none betyder ingen vej videre - ret acc

            (*let findValidMove (hand) (board:board) (dict:Dictionary.Dict) = 
                board.tiles |> Map.iter (fun key value -> uintToChar value)*)
            (*
            let findValidMove (hand) (board:board) (dict:Dictionary.Dict) = 
                let chars = 
                    board.tiles 
                    |> Seq.collect (fun (key, value) -> convertToChar value pieces)
                    |> List.ofSeq
                chars
                //board.tiles |> Map.iter (fun key value -> convertToChar value pieces)
           *)
      
            

            let rec removeUsedPiecesFromHand (ms : ((ScrabbleUtil.coord * (uint32 * (char * int))) list)) hand =
                match ms with
                | [] -> hand
                | x::xs -> removeUsedPiecesFromHand (xs) (MultiSet.removeSingle (fst (snd x)) hand)

            let rec addNewPiecesToHand (newPieces : (uint32 * uint32) list) hand =
                match newPieces with
                | [] -> hand
                | x::xs -> addNewPiecesToHand xs (MultiSet.add (fst x) (snd x) hand)

            let rec updateTiles (ms : ((coord * (uint32 * (char * int))) list)) (tiles : Map<coord, uint32>) = 
                match ms with
                | [] -> tiles
                | x::xs -> updateTiles xs (Map.add (fst x) (fst (snd x)) tiles)

            let updateState board dict playerNumber hand =
                State.mkState board dict playerNumber hand

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                let newTiles = updateTiles ms st.board.tiles
                let newBoard = Parser.mkBoard newTiles
                let handRemovedUsedPieces = removeUsedPiecesFromHand ms st.hand
                let handAddedNewPieces = addNewPiecesToHand newPieces handRemovedUsedPieces
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = updateState newBoard st.dict st.playerNumber handAddedNewPieces
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newTiles = updateTiles ms st.board.tiles
                let newBoard = Parser.mkBoard newTiles
                let st' = updateState newBoard st.dict st.playerNumber st.hand
                //let validmoves = findValidMove st.board (MultiSet.toList st.hand) st.dict
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState (board Map.empty) dict playerNumber handSet)
        