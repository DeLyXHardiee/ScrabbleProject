﻿// This file is new and hides the implementation details of the parser.

module internal Parser
    
    open ScrabbleUtil
    open StateMonad
    
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        tiles : Map<coord, uint32>
    }

    val mkBoard : Map<coord, uint32> -> board