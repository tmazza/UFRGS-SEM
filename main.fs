type Value = 
  | TmBool 
  | TmNum
  | TmFun of Value * Value
  | TmLst of Value

type Operacao = 
  | OpSo   // Soma
  | OpSu   // Subtração
  | OpMu   // Multiplicação
  | OpDi   // Divisão
  | OpMe   // Menor
  | OpMei  // Menor ou igual
  | OpIg   // Igual
  | OpDif  // Diferente
  | OpMa   // Maior 
  | OpMai  // Maior ou igual

type term =
  | TmNum   of int                    // n
  | TmBool  of bool                   // b
  | TmOp    of term * term * Operacao // e1 op e2
  | TmIf    of term * term * term     // if e1 then e2 else e3
  | TmIdent of string                 // x
  | TmApp   of term * term            // e1 e2  
  | TmFunc  of string * term          // fn x:T => e
  | TmLet   of string * term * term   // let x = e1 in e2 | TODO: substituição
  | TmLetRec of string * Value * term * term  // let rec x:T1->T2 = e1 in e2 | TODO: substituição
  (* Execeções *)
  | TmRaise                           // raise          | TODO: step
  | TmTry   of term * term            // try e1 with e2 | TODO: step
  (* Listas *)
  | TmNil                             // []     | TODO: step
  | TmCons of term * term             // e1::e2 | TODO: step
  | TmHd of term                      // hd e1  | TODO: step
  | TmTl of term                      // tl e1  | TODO: step


(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

(* Funcao auxiliar para determinar se um termo e um valor numérico *)
let isNum t = 
  match t with 
  | TmNum(n)  -> true 
  | _ -> false

(* Se termo é ou não um valor *)
let isValue t = 
  match t with
  | TmNum(n)    -> true
  | TmBool(n)   -> true
  | _ -> false

let toInt n =
  match n with
  | TmNum(n) -> n
  | _        -> 0

let toBool n =
  match n with
  | TmBool(b) -> b
  | _         -> false

// { v/x } e | TODO
let rec subFree v x e =
  match e with 
  | TmOp( e1,e2,op ) -> TmOp(subFree v x e1,subFree v x e2, op)
  | _ -> if x = e then v else e

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = 
  match t with
    (* Op *)
    | TmOp( e1,e2,op ) 
        when isNum e1 && isNum e2 ->
          match op with
          | OpSo  -> TmNum( (toInt e1) + (toInt e2) )
          | OpSu  -> TmNum( (toInt e1) - (toInt e2) )
          | OpMu  -> TmNum( (toInt e1) * (toInt e2) )
          | OpDi  -> TmNum( (toInt e1) / (toInt e2) )
          | OpMe  -> TmBool( (toInt e1) <  (toInt e2) )
          | OpMei -> TmBool( (toInt e1) <= (toInt e2) )
          | OpIg  -> TmBool( (toInt e1) =  (toInt e2) )
          | OpDif -> TmBool( (toInt e1) <> (toInt e2) )
          | OpMai -> TmBool( (toInt e1) >= (toInt e2) )
          | OpMa  -> TmBool( (toInt e1) >  (toInt e2) )

    | TmOp( e1,e2,op ) when isNum e1 ->
        let e2' = step e2 in TmOp( e1,e2',op ) 

    | TmOp( e1,e2,op ) ->
        let e1' = step e1 in TmOp( e1',e2,op )

    (* If *)
    | TmIf( TmBool(true), e2, e3 ) -> e2

    | TmIf( TmBool(false), e2, e3 ) -> e3

    | TmIf( e1, e2, e3 ) -> 
        let e1' = step e1 in TmIf( e1',e2,e3 )
    
    (* App *)
    | TmApp( TmFunc(x,e),v ) -> subFree v (TmIdent x) e

    | TmApp( e1,e2 ) when isValue e1 ->
        let e2' = step e2 in TmApp( e1,e2' )

    | TmApp( e1,e2 ) ->
        let e1' = step e1 in TmApp( e1',e2 )

    (* Let *)
    | TmLet( x,e1,e2 ) when isValue e1 -> subFree e1 (TmIdent x) e2

    | TmLet( x,e1,e2 ) ->
        let e1' = step e1 in TmLet( x,e1',e2 )

    (* NoRuleApplies *)
    | _ -> raise NoRuleApplies

(* Implementacao de EVAL *)
let rec eval t =
  try let t' = step t
      in eval t'
  with NoRuleApplies -> t


(*TESTES*)
let evalList list =
  List.map (fun x-> (fst x,eval (fst x)),(snd x) ) list

// Tuple (input,expected output)
let testes = [
  // (1 + 2) -> (3)
  TmOp(TmNum 1,TmNum 2 ,OpSo),TmNum(3);
  // (-1 + 2) -> (1)
  TmOp(TmNum -1,TmNum 2 ,OpSo),TmNum(1);
  // (1 - 2) -> (-1)
  TmOp(TmNum 1,TmNum 2 ,OpSu),TmNum(-1);
  // (2 * 2) -> (4)
  TmOp(TmNum 2,TmNum 2 ,OpMu),TmNum(4);
  // (4 / 2) -> (2)
  TmOp(TmNum 4,TmNum 2 ,OpDi),TmNum(2);
  // (1 < 2) -> (True)
  TmOp(TmNum 1,TmNum 2 ,OpMe),TmBool(true);
  // (1 <= 2) -> (True)
  TmOp(TmNum 1,TmNum 2 ,OpMei),TmBool(true);
  // (1 == 2) -> (False)
  TmOp(TmNum 1,TmNum 2 ,OpIg),TmBool(false);
  // (2 == 2) -> (True)
  TmOp(TmNum 2,TmNum 2 ,OpIg),TmBool(true);
  // (1 <> 2) -> (True)
  TmOp(TmNum 1,TmNum 2 ,OpDif),TmBool(true);
  // (1 <> 1) -> (False)
  TmOp(TmNum 1,TmNum 1 ,OpDif),TmBool(false);
  // (1 >= 2) -> (False)
  TmOp(TmNum 1,TmNum 2 ,OpMai),TmBool(false);
  // (1 > 2) -> (False)
  TmOp(TmNum 1,TmNum 2 ,OpMa),TmBool(false);
  // (if True then 1-2 else 0) -> (-1)
  TmIf( TmBool(true),TmOp(TmNum 1,TmNum 2 ,OpSu),TmNum(0) ),TmNum(-1);
  // (x) -> (x)
  TmIdent("x"),TmIdent("x");
  // (1) (1 > 2) -> (1) (false)
  TmApp( TmNum(1), TmOp(TmNum 1,TmNum 2 ,OpMa) ),TmApp( TmNum 1, TmBool false );
  // (1 < 2) (1 > 2) -> (true) (false)
  TmApp( TmOp(TmNum 1,TmNum 2 ,OpMe), TmOp(TmNum 1,TmNum 2 ,OpMa) ),TmApp( TmBool true, TmBool false  );
  // (fn x:T => x) (1) -> (1)
  TmApp( TmFunc("x",TmIdent("x")),TmNum 1 ),TmNum 1;
  // (fn x:T => x + 1) (1) -> (2)
  TmApp( TmFunc("x",TmOp(TmIdent("x"),TmNum 1 ,OpSo)),TmNum 1 ),TmNum 2;
  // (fn x:T => x + 1) -> (fn x:T => x + 1)
  TmFunc("x",TmOp(TmIdent("x"),TmNum(1) ,OpSo)),TmFunc("x",TmOp(TmIdent("x"),TmNum(1) ,OpSo)); 
  // (let x = 1 in x + 2) -> (3)
  TmLet("x",TmNum(1), TmOp(TmIdent("x"),TmNum(2),OpSo)),TmNum(3);
  // (let x = 4 - 2 in x + 2) -> (4)
  TmLet("x",TmOp(TmNum(4),TmNum(2),OpSu), TmOp(TmIdent("x"),TmNum(2),OpSo)),TmNum(4);
]
  
let prettyPrint list =
  List.map (fun x->
    if (snd (fst x) = snd x) then 
      printf "\n\tpass | %A -> %A" (fst (fst x)) (snd (fst x))
    else 
      printf "\n\tfail | test: %A | output: %A insted of %A " (fst (fst x)) (snd (fst x)) (snd x)
  ) list

let r = prettyPrint (evalList testes)
printf "\n\n"