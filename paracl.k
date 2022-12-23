module PARACL-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX
    imports ID-SYNTAX
    imports DOMAINS-SYNTAX

    syntax IntegralType ::= Bool | Int
    syntax Variable ::= Id
    syntax ValueHolder ::= IntegralType | Variable

    syntax Int ::= ToInt(ValueHolder) [function]
    syntax Bool ::= ToBool(ValueHolder) [function]
    syntax IntegralType ::= Lookup(Variable)
    syntax Exp ::= ValueHolder
                 | "(" Exp ")" [bracket]
                 | "++" Exp
                 | "--" Exp
                 | Exp "++"
                 | Exp "--"
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
                 | Variable "=" Exp [strict(2), right]
                 | Variable "+=" Exp [strict(2), right]
                 | Variable "-=" Exp [strict(2), right]
                 | Variable "*=" Exp [strict(2), right]
                 | Variable "/=" Exp [strict(2), right]
                 | Variable "%=" Exp [strict(2), right]

    syntax Block ::= "{" Stmt "}" | "{" "}"

    syntax Stmt ::= Exp ";" [strict]
                | Stmt Stmt [left]
                | Block
                | "if" "(" Exp ")" Block "else" Block  [strict(1)]
                | "if" "(" Exp ")" Block  [strict(1)]
                | "while" "(" Exp ")" Block  [strict(1)]
                | "do" Block "while" "(" Exp ")"
                | "for" "(" Exp ";" Exp ";" Exp ")" Block
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

    configuration
    <T>
        <k> $PGM:Stmt </k>
        <state> .Map </state>
    </T>

    rule <k> S1:Stmt S2:Stmt => S1 ~> S2 ... </k>
    rule _:IntegralType ; => .

    // implicit lookup
    rule <k> X:Variable => I:IntegralType ...</k>
        <state>... X |-> I ...</state>

    // explicit lookup, check if something is lvalue
    // right now is redundant, as only :Variable can be lvalue,
    // but in future it might change
    // rule <k> Lvalue(L:Id) => V ...</k> 
    //     <state>... L |-> V ...</state>

    // assignment
    rule <k> X:Variable = I:IntegralType => I ...</k>
        <state> STATE => STATE [ X <- I ] </state>

    rule <k> X:Variable += I:IntegralType => X = X + I ... </k>
    rule <k> X:Variable -= I:IntegralType => X = X - I ... </k>
    rule <k> X:Variable *= I:IntegralType => X = X * I ... </k>
    rule <k> X:Variable /= I:IntegralType => X = X / I ... </k>
    rule <k> X:Variable %= I:IntegralType => X = X % I ... </k>

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

    rule <k> A + B => {ToInt(A)}:>Int +Int {ToInt(B)}:>Int ...</k>
    rule <k> A - B  => {ToInt(A)}:>Int -Int {ToInt(B)}:>Int ...</k>
    rule <k> A * B => {ToInt(A)}:>Int *Int {ToInt(B)}:>Int ...</k>
    rule <k> A / B => {ToInt(A)}:>Int /Int {ToInt(B)}:>Int ...</k>
    rule <k> A ** B => {ToInt(A)}:>Int ^Int {ToInt(B)}:>Int ...</k>
    rule <k> A % B => {ToInt(A)}:>Int %Int {ToInt(B)}:>Int ...</k>

    rule <k> ++ X:Variable => I + 1 ...</k>
       <state>... X |-> (I => {ToInt(I)}:>Int +Int 1) ...</state>
    rule <k> -- X:Variable => I - 1 ...</k>
       <state>... X |-> (I => {ToInt(I)}:>Int -Int 1) ...</state>

    rule <k> X:Variable ++ => I ...</k>
       <state>... X |-> (I => {ToInt(I)}:>Int +Int 1) ...</state>
    rule <k> X:Variable -- => I ...</k>
       <state>... X |-> (I => {ToInt(I)}:>Int -Int 1) ...</state>

    rule <k> { S:Stmt } => S:Stmt ... </k>
    rule <k> {   } => . ... </k>
    
    rule <k> if (true:Bool) S else _ => S ... </k>
    rule <k> if (false:Bool) _ else S => S ... </k>
    rule <k> if (COND:ValueHolder) THEN:Block else ELSE:Block => if (ToBool(COND)) THEN else ELSE ... </k> [owise]

    rule <k> if (true:Bool) S  => S ... </k>
    rule <k> if (false:Bool) _ => . ... </k>
    rule <k> if (COND:ValueHolder) THEN:Block => if (ToBool(COND)) THEN ... </k> [owise]

    rule <k> while ( COND ) BODY => if ( COND ) { BODY while ( COND ) BODY } ... </k>

    rule <k> do BODY while ( COND ) => BODY while (COND) BODY ... </k>

    rule <k> for ( INIT ; STOP ; STEP ) BODY => {INIT ; while ( STOP ) { BODY STEP; }} ... </k>

    syntax KResult ::= IntegralType

endmodule
