module PARACL-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX

    syntax TypeId ::= "int" | "bool"
    syntax IntegralType ::= Bool | Int
    syntax Variable ::= Id
    syntax ValueHolders ::= IntegralType | Variable

    syntax Int ::= ToInt(IntegralType)  [function]
    syntax Bool ::= ToBool(IntegralType) [function]
    syntax Exp ::= IntegralType
                |  "(" Exp ")" [bracket]
                > left: "!" Exp [function]
                > left: Exp "**" Exp [function]
                > left: Exp "*" Exp [function]
                |       Exp "/" Exp [function]
                > left: Exp "+" Exp [function]
                |       Exp "-" Exp [function]
                > left: Exp "^" Exp [function]
                > left: Exp "&&" Exp [function]
                > left: Exp "||" Exp [function]

    syntax VarDecl ::= TypeId Id "=" Exp
    syntax Pgm ::= VarDecl

    syntax VarAssign ::= Id "=" Exp

endmodule

module PARACL
    imports INT
    imports BOOL
    imports ID
    imports PARACL-SYNTAX

    // configuration <T>
    //               <k> $PGM:Pgm </k>
    //               <state> .Map </state>
    //             </T>

    rule ToInt(I:Int) => I
    rule ToInt(false:Bool) => 0
    rule ToInt(true:Bool) => 1

    rule ToBool(B:Bool) => B
    rule ToBool(0:Int) => false
    rule ToBool(_:Int) => true [owise]

    rule ! B => notBool {ToBool(B)}:>Bool
    rule A && B => {ToBool(A)}:>Bool andBool {ToBool(B)}:>Bool
    rule A ^ B => {ToBool(A)}:>Bool xorBool {ToBool(B)}:>Bool
    rule A || B => {ToBool(A)}:>Bool orBool {ToBool(B)}:>Bool

    rule A + B => {ToInt(A)}:>Int +Int {ToInt(B)}:>Int
    rule A - B => {ToInt(A)}:>Int -Int {ToInt(B)}:>Int
    rule A * B => {ToInt(A)}:>Int *Int {ToInt(B)}:>Int
    rule A / B => {ToInt(A)}:>Int /Int {ToInt(B)}:>Int
    rule A ** B => {ToInt(A)}:>Int ^Int {ToInt(B)}:>Int

    // // declaration sequence
    // rule <k> D:VarDecl P:Pgm => D ~> P ...</k>
    // rule <k> .Pgm => . ...</k>

    // // variable declaration
    // rule <k> int X:Id = I:Int ; => . ...</k>
    //      <state> STATE => STATE [ X <- I ] </state>

    // // variable lookup
    // rule <k> X:Id => I ...</k>
    //    <state>... X |-> I ...</state>
    
endmodule
