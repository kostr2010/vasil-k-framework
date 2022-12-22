module PARACL-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX
    imports DOMAINS-SYNTAX

    syntax TypeId ::= "int" | "bool"
    syntax TypeIds ::= List{TypeId,","}

    syntax IntegralType ::= Bool | Int
    syntax Variable ::= Id
    syntax ValueHolder ::= IntegralType | Variable

    // basic expressions
    syntax Int ::= ToInt(ValueHolder) [function]
    syntax Bool ::= ToBool(ValueHolder) [function]
    syntax IntegralType ::= Lookup(Variable)
    syntax Exp ::= ValueHolder
                 | "(" Exp ")" [bracket]
                 > left: 
                   "!" Exp [strict]
                 > left: 
                   Exp "**" Exp [strict]
                 > left: 
                   Exp "*" Exp [strict]
                 | Exp "/" Exp [strict]
                 | Exp "%" Exp [strict]
                 > left: 
                   Exp "+" Exp [strict]
                 | Exp "-" Exp [strict]
                 > non-assoc: 
                   Exp "<" Exp [strict]
                 | Exp "<=" Exp [strict]
                 | Exp ">" Exp [strict]
                 | Exp ">=" Exp [strict]
                 | Exp "==" Exp [strict]
                 | Exp "!=" Exp [strict]
                 > left: 
                   Exp "^" Exp [strict]
                 > left: 
                   Exp "&&" Exp [strict]
                 > left: 
                   Exp "||" Exp [strict]

    // define syntax for blocks of code
    syntax Assign ::= Variable "=" Exp [strict(2)]
    syntax Stmt ::= Assign
                | Stmt Stmt [left]
                | Block
                | "if" "(" Exp ")" Block "else" Block  [strict(1)]
                | "if" "(" Exp ")" Block  [strict(1)]
                | "while" "(" Exp ")" Block  [strict(1)]
                | "for" "(" Stmt ";" Exp ";" Exp ")" Block  [strict]
    syntax Block ::= "{" Stmt "}" | "{" "}"

    // syntax Stmts ::= List{Stmt,""} [strict]
endmodule

module PARACL
    imports INT
    imports BOOL
    imports ID
    imports STRING
    imports MAP
    imports LIST
    imports PARACL-SYNTAX
    imports DOMAINS

    // rule {S:Stmts} => S:Stmts
    // desugaring
    // rule if (B:Exp) E1:Block else E2:Exp => #if B #then E1 #else E2 #fi
    // rule S1:Stmt S2:Stmt => S1 ~> S2  [structural]
    // rule if (E) S => if (E) S else {}

    // rule for(Start; Cond; Step) {S} => Start while (Cond) {S Step}
    // rule for(Start; Cond; Step) {} => Start while (Cond) {Step}
    // rule while (E) S => if (E) {S while(E) {S} }

    configuration
    <T>
        <k> $PGM:Stmt </k>
        <state> .Map </state>
    </T>

    rule <k> S1:Stmt S2:Stmt => S1 ~> S2 ... </k>

    rule <k> { S } => S ... </k>
    rule <k> {   } => . ... </k>
    
    rule <k> if (true:Bool) S else _ => S ... </k>
    rule <k> if (false:Bool) _ else S => S ... </k>
    rule <k> if (E:ValueHolder) THEN else ELSE => if (ToBool(E)) THEN else ELSE ... </k> [owise]

    rule <k> if (true:Bool) S  => S ... </k>
    rule <k> if (false:Bool) _ => . ... </k>
    rule <k> if (E:ValueHolder) THEN => if (ToBool(E)) THEN ... </k> [owise]

    // variable declaration
    rule <k> X:Id = I:IntegralType => . ...</k>
        <state> STATE => STATE [ X <- I ] </state> [assign]

    // variable lookup
    rule <k> X:Id => I:IntegralType ...</k>
        <state>... X |-> I ...</state> [lookup]

    // rule <k> Lookup(L:Id) => V ...</k> 
    //     <state>... L |-> V ...</state> [lookup]

    // rule ToInt(I:Id) => ToInt(Lookup(I:Id))
    rule ToInt(I:Int) => I
    rule ToInt(false:Bool) => 0
    rule ToInt(true:Bool) => 1

    // rule ToBool(B:Id) => ToBool(lookup(B:Id))
    rule ToBool(B:Bool) => B
    rule ToBool(0:Int) => false
    rule ToBool(_:Int) => true [owise]

    rule <k> ! B => notBool {ToBool(B)}:>Bool ...</k>
    rule <k> A && B => {ToBool(A)}:>Bool andBool {ToBool(B)}:>Bool ...</k>
    rule <k> A ^ B => {ToBool(A)}:>Bool xorBool {ToBool(B)}:>Bool ...</k>
    rule <k> A || B => {ToBool(A)}:>Bool orBool {ToBool(B)}:>Bool ...</k>
    
    rule <k> A < B => {ToInt(A)}:>Int <Int {ToInt(B)}:>Int ...</k>
    rule <k> A > B => {ToInt(A)}:>Int >Int {ToInt(B)}:>Int ...</k>
    rule <k> A <= B => {ToInt(A)}:>Int <=Int {ToInt(B)}:>Int ...</k>
    rule <k> A >= B => {ToInt(A)}:>Int >=Int {ToInt(B)}:>Int ...</k>
    rule <k> A == B => {ToInt(A)}:>Int ==Int {ToInt(B)}:>Int ...</k>
    rule <k> A != B => {ToInt(A)}:>Int =/=Int {ToInt(B)}:>Int ...</k>

    rule <k> A + B  => {ToInt(A)}:>Int +Int {ToInt(B)}:>Int ...</k>
    rule <k> A - B  => {ToInt(A)}:>Int -Int {ToInt(B)}:>Int ...</k>
    rule <k> A * B  => {ToInt(A)}:>Int *Int {ToInt(B)}:>Int ...</k>
    rule <k> A / B  => {ToInt(A)}:>Int /Int {ToInt(B)}:>Int ...</k>
    rule <k> A ** B => {ToInt(A)}:>Int ^Int {ToInt(B)}:>Int ...</k>
    rule <k> A % B  => {ToInt(A)}:>Int %Int {ToInt(B)}:>Int ...</k>

    syntax KResult ::= IntegralType

endmodule
