module PARACL-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX

    // syntax #Layout ::= r"(;[^\\n\\r]*)"
    //                  | r"([\\ \\n\\r\\t])"

    syntax TypeId ::= "int"
    syntax IntegralType ::= Bool | Int
    syntax Variable ::= Id
    syntax ValueHolder ::= IntegralType | Variable

    // syntax SLComment ::= r"^\/\/((\\\n)|[^\n])*"
    // syntax MLComment ::= r"^\/\*(.*)\*\/"
    // syntax Comment ::= SLComment | MLComment
    // syntax EOL ::= "" | Comment

    syntax Int ::= ToInt(IntegralType) [function]
    syntax Bool ::= ToBool(IntegralType) [function]
    syntax Exp ::= ValueHolder
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
    // syntax Exp ::= Id | Int | Bool

    syntax VarDecl ::= "int" Variable "=" Exp [strict(2)]
                     | "bool" Variable "=" Exp [strict(2)]
    syntax VarAssign ::= Variable "=" Exp

    syntax Pgm ::= List{VarDecl, ""}

endmodule

module PARACL
    imports INT
    imports BOOL
    imports ID
    imports MAP
    imports LIST
    imports PARACL-SYNTAX

    configuration <T>
                  <k> $PGM:Pgm </k>
                  <state> .Map </state>
                </T>

    rule ToInt(I:Int) => I
    rule ToInt(false:Bool) => 0
    rule ToInt(true:Bool) => 1
    // rule <k> X:Id => A ...</k>
    //      <state>... X |-> A ...</state>

    rule ToBool(B:Bool) => B
    rule ToBool(0:Int) => false
    rule ToBool(_:Int) => true [owise]
    // rule <k> X:Id => A ...</k>
    //      <state>... X |-> A ...</state>

    rule ! B => notBool {ToBool(B)}:>Bool
    rule A && B => {ToBool(A)}:>Bool andBool {ToBool(B)}:>Bool
    rule A ^ B => {ToBool(A)}:>Bool xorBool {ToBool(B)}:>Bool
    rule A || B => {ToBool(A)}:>Bool orBool {ToBool(B)}:>Bool
    rule A:Int + B:Int => {ToInt(A)}:>Int +Int {ToInt(B)}:>Int

    // rule <k> X:Id + Y:Id => A +Int B ...</k> 
    //      <state>... X |-> A ...</state>
    //      <state>... Y |-> B ...</state>

    rule A - B => {ToInt(A)}:>Int -Int {ToInt(B)}:>Int
    rule A * B => {ToInt(A)}:>Int *Int {ToInt(B)}:>Int
    rule A / B => {ToInt(A)}:>Int /Int {ToInt(B)}:>Int
    rule A ** B => {ToInt(A)}:>Int ^Int {ToInt(B)}:>Int

    // declaration sequence
    rule <k> D:VarDecl P:Pgm => D ~> P ...</k>
    rule <k> .Pgm => . ...</k>

    // variable declaration
    rule <k> int X:Id = I:Int => Exp ...</k>
         <state> STATE => STATE [ X <- I ] </state>
    
    rule <k> bool Y:Id = B:Bool => Exp ...</k>
         <state> STATE => STATE [ Y <- B ] </state>

    // variable lookup
    rule <k> X:Id => A ...</k>
         <state>... X |-> A ...</state>

    // rule <k> T:Type X:Id; => stmt ...</k>
    //         <gtenv> Rho (.Map => X |-> T) </gtenv>
    //     requires notBool(X in keys(Rho))

    // rule <k> T:Type X:Id; => stmt ...</k> <tenv> Rho => Rho[X <- T] </tenv>


    syntax Bool ::= isKResult(K) [symbol, function]
    rule isKResult(_:IntegralType) => true
    rule isKResult(_) => false [owise]
    
endmodule
