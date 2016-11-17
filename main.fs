type Value = TmBool | TmNum

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
  | TmNum    of int                    // n
  | TmBool   of bool                   // b
  | TmOp       of term * term * Operacao // e1 op e2
  | TmIf       of term * term * term     // if e1 then e2 else e3
  | TmIdent    of string                 // x
  | TmApp      of term * term            // e1 e2
  | TmFunc     of string * term          // fn x:T => e


(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isNumValue t = 
  match t with
    | TmNum(n)    -> true
    | TmBool(n)   -> true
    | _           -> false

let toInt n =
  match n with
    | TmNum(n) -> n
    | _        -> 0

let toBool n =
  match n with
    | TmBool(b) -> b
    | _         -> false

// { v/x } e | TODO
let rec substitui x e v =
  e

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = 
  match t with
    (* Op *)
    | TmOp( e1,e2,op ) 
        when isNumValue e1 && isNumValue e2 ->
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

    | TmOp( e1,e2,op ) when isNumValue e1 ->                             // E-op2
        let e2' = step e2 in TmOp( e1,e2',op ) 

    | TmOp( e1,e2,op )                 ->                             // E-op1
        let e1' = step e1 in TmOp( e1',e2,op )

    (* If *)
    | TmIf( TmBool(true), e2, e3 )   -> e2                          // E−IfTrue
    | TmIf( TmBool(false), e2, e3 )  -> e3                          // E−IfFalse
    | TmIf( e1, e2, e3 )               -> 
        let e1' = step e1 in TmIf( e1',e2,e3 )                        // E-if
    
    (* App *)
    | TmApp( TmFunc(x,e),v ) -> substitui x e v                       // E-Beta

    | TmApp( e1,e2 ) when isNumValue e1   ->                             // E-App1
        let e2' = step e2 in TmApp( e1,e2' )
    | TmApp( e1,e2 )                   ->                             // E-App1
        let e1' = step e1 in TmApp( e1',e2 )

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
  TmOp(TmNum 1,TmNum 2 ,OpSo)          ,TmNum(3);
  TmOp(TmNum 1,TmNum 2 ,OpSo)          ,TmNum(1);
  TmOp(TmNum 1,TmNum 2 ,OpSu)     ,TmNum(-1);
  TmOp(TmNum 2,TmNum 2 ,OpMu) ,TmNum(4);
  TmOp(TmNum 4,TmNum 2 ,OpDi)       ,TmNum(2);
  TmOp(TmNum 1,TmNum 2 ,OpMe)         ,TmBool(true);
  TmOp(TmNum 1,TmNum 2 ,OpMei)  ,TmBool(true);
  TmOp(TmNum 1,TmNum 2 ,OpIg)         ,TmBool(false);
  TmOp(TmNum 2,TmNum 2 ,OpIg)         ,TmBool(true);
  TmOp(TmNum 1,TmNum 2 ,OpDif)     ,TmBool(true);
  TmOp(TmNum 1,TmNum 1 ,OpDif)     ,TmBool(false);
  TmOp(TmNum 1,TmNum 2 ,OpMai)  ,TmBool(false);
  TmOp(TmNum 1,TmNum 2 ,OpMa)         ,TmBool(false);
  TmIf( TmBool(true),TmOp(TmNum 1,TmNum 2 ,OpSu),TmNum(0) ),TmNum(-1);
  TmIdent("x"),TmIdent("x");
  // (1) (1 > 2) -> (1) (false)
  TmApp( TmNum(1), TmOp(TmNum 1,TmNum 2 ,OpMa) ),TmApp( TmNum 1, TmBool false );
  // (1 < 2) (1 > 2) -> (true) (false)
  TmApp( TmOp(TmNum 1,TmNum 2 ,OpMe), TmOp(TmNum 1,TmNum 2 ,OpMa) ),TmApp( TmBool true, TmBool false  );
  // (fn x => x + 1) (1) -> (2)
  TmApp( TmFunc("x",TmOp(TmIdent("x"),TmNum 1 ,OpSo)),TmNum 1 ),TmNum 2;
  // fn x: => x + 1
  //TmFunc("x",TmOp(TmIdent("x"),TmNum(1) ,OpSo)),TmNum(2); 
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