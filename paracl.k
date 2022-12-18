module PARACL-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX

    syntax IntegralType ::= Bool | Int

    syntax Bool ::= ToBool(IntegralType) [function]
    syntax BoolExp ::= IntegralType
                    | "(" BoolExp ")" [bracket]
                    > "!" BoolExp [function]
                    > left: BoolExp "&&" BoolExp [function]
                    > left: BoolExp "^" BoolExp [function]
                    > left: BoolExp "||" BoolExp [function]

    syntax Int ::= ToInt(IntegralType)  [function]
    syntax IntExp ::= IntegralType
                  |  "(" IntExp ")" [bracket]
                  > left: IntExp "*" IntExp [function]
                  | IntExp "**" IntExp [function]
                  | IntExp "/" IntExp [function]
                  > left: IntExp "+" IntExp [function]
                  | IntExp "-" IntExp [function]

endmodule

module PARACL
    imports INT
    imports BOOL
    imports ID
    imports PARACL-SYNTAX

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
    
endmodule
