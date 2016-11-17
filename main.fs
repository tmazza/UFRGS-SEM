type Value = TipoBool | TipoInt

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
  | TipoInt    of int                    // n
  | TipoBool   of bool                   // b
  | TmOp       of term * term * Operacao // e1 op e2
  | TmIf       of term * term * term     // if e1 then e2 else e3
  | TmIdent    of string                 // x
  | TmApp      of term * term            // e1 e2
  | TmFunc     of string * term          // fn x:T => e


(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isValue t = 
  match t with
    | TipoInt(n)    -> true
    | TipoBool(n)   -> true
    | _             -> false

let toInt n =
  match n with
    | TipoInt(n) -> n
    | _          -> 0

let toBool n =
  match n with
    | TipoBool(b) -> b
    | _           -> false

// { v/x } e | TODO
let rec substitui x e v =
  e

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = 
  match t with
    (* Op *)
    | TmOp( e1,e2,e3 ) 
        when isValue e1 && isValue e2 ->
          match e3 with
          | OpSo          -> TipoInt( (toInt e1) + (toInt e2) )
          | OpSu     -> TipoInt( (toInt e1) - (toInt e2) )
          | OpMu -> TipoInt( (toInt e1) * (toInt e2) )
          | OpDi       -> TipoInt( (toInt e1) / (toInt e2) )
          | OpMe         -> TipoBool( (toInt e1) <  (toInt e2) )
          | OpMei  -> TipoBool( (toInt e1) <= (toInt e2) )
          | OpIg         -> TipoBool( (toInt e1) =  (toInt e2) )
          | OpDif     -> TipoBool( (toInt e1) <> (toInt e2) )
          | OpMai  -> TipoBool( (toInt e1) >= (toInt e2) )
          | OpMa         -> TipoBool( (toInt e1) >  (toInt e2) )

    | TmOp( e1,e2,e3 ) when isValue e1 ->                             // E-op2
        let e2' = step e2 in TmOp( e1,e2',e3 ) 

    | TmOp( e1,e2,e3 )                 ->                             // E-op1
        let e1' = step e1 in TmOp( e1',e2,e3 )

    (* If *)
    | TmIf( TipoBool(true), e2, e3 )   -> e2                          // E−IfTrue
    | TmIf( TipoBool(false), e2, e3 )  -> e3                          // E−IfFalse
    | TmIf( e1, e2, e3 )               -> 
        let e1' = step e1 in TmIf( e1',e2,e3 )                        // E-if
    
    (* App *)
    | TmApp( TmFunc(x,e),v ) -> substitui x e v                       // E-Beta

    | TmApp( e1,e2 ) when isValue e1   ->                             // E-App1
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
  TmOp(TipoInt 1,TipoInt 2 ,OpSo)          ,TipoInt(3);
  TmOp(TipoInt 1,TipoInt 2 ,OpSo)          ,TipoInt(1);
  TmOp(TipoInt 1,TipoInt 2 ,OpSu)     ,TipoInt(-1);
  TmOp(TipoInt 2,TipoInt 2 ,OpMu) ,TipoInt(4);
  TmOp(TipoInt 4,TipoInt 2 ,OpDi)       ,TipoInt(2);
  TmOp(TipoInt 1,TipoInt 2 ,OpMe)         ,TipoBool(true);
  TmOp(TipoInt 1,TipoInt 2 ,OpMei)  ,TipoBool(true);
  TmOp(TipoInt 1,TipoInt 2 ,OpIg)         ,TipoBool(false);
  TmOp(TipoInt 2,TipoInt 2 ,OpIg)         ,TipoBool(true);
  TmOp(TipoInt 1,TipoInt 2 ,OpDif)     ,TipoBool(true);
  TmOp(TipoInt 1,TipoInt 1 ,OpDif)     ,TipoBool(false);
  TmOp(TipoInt 1,TipoInt 2 ,OpMai)  ,TipoBool(false);
  TmOp(TipoInt 1,TipoInt 2 ,OpMa)         ,TipoBool(false);
  TmIf( TipoBool(true),TmOp(TipoInt 1,TipoInt 2 ,OpSu),TipoInt(0) ),TipoInt(-1);
  TmIdent("x"),TmIdent("x");
  // (1) (1 > 2) -> (1) (false)
  TmApp( TipoInt(1), TmOp(TipoInt 1,TipoInt 2 ,OpMa) ),TmApp( TipoInt 1, TipoBool false );
  // (1 < 2) (1 > 2) -> (true) (false)
  TmApp( TmOp(TipoInt 1,TipoInt 2 ,OpMe), TmOp(TipoInt 1,TipoInt 2 ,OpMa) ),TmApp( TipoBool true, TipoBool false  );
  // (fn x => x + 1) (1) -> (2)
  TmApp( TmFunc("x",TmOp(TmIdent("x"),TipoInt 1 ,OpSo)),TipoInt 1 ),TipoInt 2;
  // fn x: => x + 1
  //TmFunc("x",TmOp(TmIdent("x"),TipoInt(1) ,OpSo)),TipoInt(2); 
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