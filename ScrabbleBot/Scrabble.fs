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

            let convertToPoints (value:uint32) (pieces:Map<uint32, tile>) : int = 
                let points = match Map.tryFind value pieces with
                                        | Some tile -> match Set.maxElement tile with
                                                            | piece -> snd piece
                                        | None -> failwith "Not a valid character"
                points

            let uintToChar id = char(id + 64u)
            let charToUint c = uint32(c)-64u
            
            // Call when you have found starter and propegate dictionary into findValidWord
            let stepIntoWord (word:string) (dict:Dictionary.Dict) : Dictionary.Dict =
                let characters = Seq.toList word
                let rec aux (c:char list) (startDict:Dictionary.Dict)=
                    match characters with
                    | x::xs ->  
                        match Dictionary.step x startDict with
                        | Some (_,subDict: Dictionary.Dict) -> aux xs subDict 
                        | None -> startDict
                    | [] -> startDict
                aux characters dict
                    
            let findValidWord (hand:MultiSet.MultiSet<uint32>) (startDict:Dictionary.Dict) (maxLength : int) (startString:string) : string =
                let rec aux (hand:MultiSet.MultiSet<uint32>) (auxDict:Dictionary.Dict) (maxLength : int) (currentString:string) : string = 
                    if currentString.Length > (startString.Length + maxLength) then ""
                    else
                    MultiSet.fold (fun acc letter count -> 
                        let newHand = MultiSet.removeSingle letter hand
                        let letter = 
                            match letter with
                            | 0u -> 5u
                            | _ -> letter
                        match Dictionary.step (uintToChar letter) auxDict with 
                        | Some (endOfWord, subDict) ->
                            let newString = currentString + (string (uintToChar letter))
                            let branch = aux newHand subDict maxLength newString
                            if endOfWord && newString.Length >= branch.Length && newString.Length >= acc.Length && newString.Length <= (startString.Length + maxLength) &&  (Dictionary.lookup newString st.dict) then
                                //printf "endOfWord: %b \n" endOfWord
                                //printf "startString: %s \n " startString
                                //printf "currentString: %s \n " currentString
                                newString
                            elif branch.Length > acc.Length && branch.Length < (startString.Length + maxLength) && (Dictionary.lookup branch st.dict) then 
                                //printf "branch: %s \n " branch
                                branch
                            elif (Dictionary.lookup acc st.dict) then 
                                //printf "acc: %s \n " acc
                                acc 
                            else ""
                        | None -> acc
                    ) startString hand
                aux hand startDict maxLength startString


            (*let findValidWord (hand:MultiSet.MultiSet<uint32>) (dict:Dictionary.Dict) (maxLength:int) =
                let hand = MultiSet.toList hand
                let rec aux (hand:uint32 list) (dict:Dictionary.Dict) (maxLength:int) (acc:string) =
                    List.fold (fun acc letter ->  
                        match Dictionary.step (uintToChar letter) dict with
                        | Some (endOfWord,subdict) when endOfWord = true -> acc + (string (uintToChar letter))
                        | Some (endOfWord,subdict) -> 
                            let newHand = 
                                match hand with 
                                | x::xs -> xs
                                | [] -> List.empty  
                            aux newHand subdict maxLength (acc + (string (uintToChar letter)))
                        | None -> ""
                    ) acc hand
                aux hand dict maxLength ""*)

            let makeMove (startPos:coord) (direction:coord) (word:string) (hand:MultiSet.MultiSet<uint32>) = 
                let rec moveHelper (pos:coord) direction remainingWord (moves:list<coord * (uint32 * (char * int))>) (hand:MultiSet.MultiSet<uint32>) =
                    let newPos:coord =
                        if Coord.getX direction = 1 then
                            Coord.mkCoordinate (Coord.getX pos + 1) (Coord.getY pos)
                        else
                            Coord.mkCoordinate (Coord.getX pos) (Coord.getY pos + 1)
                    match remainingWord with
                    | [] -> moves
                    | w::xs -> 
                        let move = 
                            match MultiSet.contains (charToUint w) hand with
                            | true -> 
                                (pos,(charToUint (char(w)),(char(w),convertToPoints (charToUint(w)) pieces)))
                            | false -> 
                                (pos,((0u),(char(w),0)))
                        let newHand = 
                            match MultiSet.contains (charToUint w) hand with
                            | true -> MultiSet.removeSingle (charToUint w) hand
                            | false -> MultiSet.removeSingle (0u) hand
                        moveHelper newPos direction xs (move :: moves) newHand
                moveHelper startPos direction (Seq.toList(word)) [] hand

            //helper function to find nearby letters that may interfere with the move
            let findAdjacent (coord: coord) (direction: coord) =
                let next = Coord.mkCoordinate(Coord.getX coord + Coord.getX direction) (Coord.getY coord + Coord.getY direction)
                if Coord.getX direction = 1 then [((Coord.getX next, ((Coord.getY next) - 1))); (Coord.getX next, (Coord.getY next) + 1); ((Coord.getX next) + 1, Coord.getY next)]
                else [((Coord.getX next) - 1, (Coord.getY next)); ((Coord.getX next) + 1, Coord.getY next); ((Coord.getX next), (Coord.getY next) + 1)]

            //helper function used in combination with findadjacent
            let rec isAvailable (coords: coord List) (board: board) =
                match coords with 
                | x::xs -> match Map.tryFind x board.tiles with 
                            | Some _ -> false
                            | None -> isAvailable xs board
                | [] -> true

            //helper function to find out how many characters we can place after some anchorpoint on the board without collision
            let findLengthFromStarter (coord:coord) (direction:coord) (board:board) = 
                let rec aux coord direction board acc = 
                    match isAvailable (findAdjacent coord direction) board with
                    | true when acc < 7 -> aux (Coord.mkCoordinate(Coord.getX coord + Coord.getX direction) (Coord.getY coord + Coord.getY direction)) direction board (acc + 1)
                    | true -> acc
                    | false -> acc
                aux coord direction board 0

            //helper function to find the suffix of the anchor point. Includes the character at the anchor point
            let rec successor (coord:coord) (direction:coord) (tiles : Map<coord,uint32>) = 
                let rec aux coord direction tiles acc =
                    let nextCoord = (Coord.mkCoordinate(Coord.getX coord + Coord.getX direction) (Coord.getY coord + Coord.getY direction))
                    match Map.tryFind nextCoord tiles with
                    | Some x -> aux nextCoord direction tiles (acc + (string (uintToChar x)))
                    | None -> (acc,coord)
                aux coord direction tiles (string (uintToChar (Map.find (coord) tiles)))
            
            //helper function to find the prefix of the anchor point
            let rec predecessor coord direction tiles =
                let rec aux coord direction tiles acc =
                    let prevCoord = (Coord.mkCoordinate(Coord.getX coord - Coord.getX direction) (Coord.getY coord - Coord.getY direction))
                    match Map.tryFind prevCoord tiles with
                    | Some x -> aux prevCoord direction tiles ((string (uintToChar x) + acc))
                    | None -> acc
                aux coord direction tiles ""

            //helper function to find the string from some anchor point that we will be progressing from. Is often just the anchor point, i.e. some character placed on the board.
            //also returns the last coordinate of the placement of the string, which is where we will continue building from.
            let startSubString (coord : coord)(direction : coord) (board : board) =
                let tiles = board.tiles
                let beginSubString = predecessor coord direction tiles
                let endSubString = successor coord direction tiles
                ((beginSubString + fst endSubString), snd endSubString)


            //helper function to finding valid moves. We match all pieces placed on the board, and use each anchor point to find a valid move.
            //as soon as a valid move is found from some anchor point, we use it.
            let findValidMoveHelper hand dict placedPieces board  direction  =
                let shuffleList (list: 'T list) : 'T list =
                    let rand = System.Random()
                    List.sortBy (fun _ -> rand.Next()) list
                let placedPieces = shuffleList placedPieces
                let rec aux hand placedPieces dict direction=
                    match placedPieces with 
                    | x::xs -> 
                        let subStringAndLastCoord = startSubString (fst x) direction board
                        let subString = fst subStringAndLastCoord
                        // printf "looking at coordinate: %i, %i \n" (fst (fst x)) (snd (fst x))
                        // printf "sub string: %s \n" subString
                        let dictionaryDepth = stepIntoWord subString dict
                        let lastCoordInSubString = snd subStringAndLastCoord
                        // printf "last coordinate: %i, %i \n" (fst lastCoordInSubString) (snd lastCoordInSubString)
                        let lengthFromStarter = findLengthFromStarter lastCoordInSubString direction board
                        // printf "length from starter: %i \n" lengthFromStarter
                        let word = findValidWord hand dictionaryDepth lengthFromStarter subString
                        //printf "our valid word! %s \n" word
                        let wordWithoutAlreadyPlaced = word[subString.Length .. word.Length]
                        // printf "word: %s \n" wordWithoutAlreadyPlaced
                        if (word.Length < 1 || Dictionary.lookup word dict) then 
                            let move = makeMove (Coord.mkCoordinate (Coord.getX lastCoordInSubString + Coord.getX direction) (Coord.getY lastCoordInSubString + Coord.getY direction)) direction wordWithoutAlreadyPlaced hand
                            if move.IsEmpty then aux hand xs dict direction
                            else move
                        else aux hand xs dict direction
                    | [] -> list.Empty
                aux hand placedPieces dict direction
    
            //finds some valid move by using findValidMoveHelper. If no move has been made yet, place one beginning in (0,0) going right.
            let findValidMove (hand) (board:board) (dict:Dictionary.Dict) = 
                let list = Map.toList board.tiles
                let directionDown = (Coord.mkCoordinate 0 1)
                let directionRight =  (Coord.mkCoordinate 1 0) 
                if list.IsEmpty then makeMove (0, 0) (1, 0) (findValidWord st.hand st.dict 7 "") hand
                else
                    let findValidMoveRight = findValidMoveHelper hand dict list board directionRight 
                    //printf "found right\n"
                    let findValidMoveDown = findValidMoveHelper hand dict list board directionDown
                    //printf "found down\n"
                    if findValidMoveRight.IsEmpty && findValidMoveDown.IsEmpty then List.empty //no possible move, switch pieces
                    elif findValidMoveRight.IsEmpty then findValidMoveDown
                    elif findValidMoveDown.IsEmpty then findValidMoveRight
                    elif findValidMoveRight.Length > findValidMoveDown.Length then findValidMoveRight
                    else findValidMoveDown
                    

            (* let rec aux hand placedPieces dict =
                        match placedPieces with 
                        | x::xs -> 
                            let subString = startSubString (fst x) (Coord.mkCoordinate 1 0) board
                            let dictionaryDepth = stepIntoWord subString dict
                            let lastCoordInSubString = Coord.mkCoordinate((fst (fst x)) + (1 * subString.Length)) (snd (fst x))
                            let lengthFromStarter = findLengthFromStarter lastCoordInSubString (Coord.mkCoordinate 1 0) board
                            let word = findValidWord hand dictionaryDepth lengthFromStarter subString
                            let move = makeMove (Coord.mkCoordinate(fst (lastCoordInSubString) + 1) (snd lastCoordInSubString)) (Coord.mkCoordinate 1 0) word
                            if move.IsEmpty then aux hand xs dict
                            else move
                        | [] -> failwith "No valid move, change pieces!" *)

            let move = findValidMove st.hand st.board st.dict 
            //forcePrint (string move)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            let tilesInBag = 100-(Map.count st.board.tiles) - (int (MultiSet.size st.hand))
            //printf "pieces used: %s \n" (string (Map.count st.board.tiles))
            let tilesToChange = (MultiSet.toList st.hand)[0..tilesInBag] 
            let handAfterChange = List.fold (fun acc x -> MultiSet.removeSingle x acc) st.hand tilesToChange
            if move.IsEmpty then 
                let list = MultiSet.toList (st.hand)
                //printf "tilesInBag: %s \n" (string tilesInBag)
                //forcePrint ("hand size when changing pieces: ")
                //forcePrint ((string list.Length)+ "\n")
                send cstream (SMChange (tilesToChange))
                
            else send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                

            let rec removeUsedPiecesFromHand (ms : ((ScrabbleUtil.coord * (uint32 * (char * int))) list)) hand =
                match ms with
                | [] -> hand
                | x::xs -> removeUsedPiecesFromHand (xs) (MultiSet.removeSingle (fst (snd x)) hand)

            let rec addNewPiecesToHand (newPieces : (uint32 * uint32) list) hand =
                match newPieces with
                | [] -> hand
                | x::xs -> addNewPiecesToHand xs (MultiSet.add (fst x) (snd x) hand)

            let changePiecesInHand (newPieces : (uint32 * uint32) list) =
                addNewPiecesToHand newPieces handAfterChange

            let rec updateTiles (ms : ((coord * (uint32 * (char * int))) list)) (tiles : Map<coord, uint32>) = 
                match ms with
                | [] -> tiles
                | x::xs -> updateTiles xs (Map.add (fst x) (fst (snd x)) tiles)

            let updateState board dict playerNumber hand =
                State.mkState board dict playerNumber hand

            //printf "response: %s\n" (string msg)

            match msg with
            | RCM (CMChangeSuccess(newPieces)) ->
                //printf "Changing to new pieces! \n"
                //printf "Old hand: %s \n" (string st.hand) 
                let handChangedToNewPieces = changePiecesInHand newPieces
                let st' = updateState st.board st.dict st.playerNumber handChangedToNewPieces
                //printf "New hand: %s \n" (string st'.hand)
                aux st'
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //printf "Hand: %s \n" (string st.hand) 
                forcePrint ("pieces played: " + (string (ms.Length) + "\n"))
                forcePrint ("new pieces size: " + (string ((newPieces.Length))) + "\n")
                let newTiles = updateTiles ms st.board.tiles
                let newBoard = Parser.mkBoard newTiles
                let handRemovedUsedPieces = removeUsedPiecesFromHand ms st.hand
                let handAddedNewPieces = addNewPiecesToHand newPieces handRemovedUsedPieces
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = updateState newBoard st.dict st.playerNumber handAddedNewPieces
                forcePrint ("pieces played: " + (string (ms.Length) + "\n"))
                forcePrint ("handsize after removing played pieces: " + (string (MultiSet.size(handRemovedUsedPieces))) + "\n")
                
                //printf "board size: %s \n" (string (Map.count newBoard.tiles))
                //printf "making a new move!\n"
                //printf "hand size after play: %s \n" (string (MultiSet.size st'.hand))
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //printf "move was made!\n"
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
            | RCM a -> aux st
            | RGPE err -> 
                //printfn "Gameplay Error:\n%A" err; 
                send cstream (SMPass)
                aux st

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
        