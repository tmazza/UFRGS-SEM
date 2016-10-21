type Value = TmBool | TmInt

type Operacao = 
  | OpSoma
  | OpSubtracao
  | OpMultiplicacao
  | OpDivisao
  | OpMenor
  | OpMenorOuIgual
  | OpIgual
  | OpDiferente
  | OpMaiorOuIgual
  | OpMaior

type term =
  | Value
  | TmInt    of n:int
  | TmBool   of b:bool
  | TmOp     of term * term * Operacao
  | TmIf     of term * term * term


(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
exception NoRuleApplies

(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isValue t = 
  match t with
    | TmInt(n)    -> true
    | _           -> false

let toInt n =
  match n with
    | TmInt(n) -> n
    | _        -> 0

let toBool n =
  match n with
    | TmBool(b) -> b
    | _         -> false

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = 
  match t with
    (* Op *)
    | TmOp( e1,e2,e3 ) 
        when isValue e1 && isValue e2 ->
          match e3 with
          | OpSoma          -> TmInt( (toInt e1) + (toInt e2) )
          | OpSubtracao     -> TmInt( (toInt e1) - (toInt e2) )
          | OpMultiplicacao -> TmInt( (toInt e1) * (toInt e2) )
          | OpDivisao       -> TmInt( (toInt e1) / (toInt e2) )
          | OpMenor         -> TmBool( (toInt e1) <  (toInt e2) )
          | OpMenorOuIgual  -> TmBool( (toInt e1) <= (toInt e2) )
          | OpIgual         -> TmBool( (toInt e1) =  (toInt e2) )
          | OpDiferente     -> TmBool( (toInt e1) <> (toInt e2) )
          | OpMaiorOuIgual  -> TmBool( (toInt e1) >= (toInt e2) )
          | OpMaior         -> TmBool( (toInt e1) >  (toInt e2) )

    | TmOp( e1,e2,e3 ) when isValue e1 ->                             // E-op2
        let e2' = step e2 in TmOp( e1,e2',e3 ) 

    | TmOp( e1,e2,e3 ) ->                                             // E-op1
        let e1' = step e1 in TmOp( e1',e2,e3 )

    (* If *)
    | TmIf( TmBool(true), t2, t3 )    -> t2                           // E−IfTrue
    | TmIf( TmBool(false), t2, t3 )   -> t3                           // E−IfFalse
    | TmIf( t1, t2, t3 )              -> 
        let t1' = step t1 in TmIf( t1',t2,t3 )                        // E-if
    
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
  TmOp(TmInt 1,TmInt 2 ,OpSoma)               ,TmInt(3);
  TmOp(TmInt 1,TmInt 2 ,OpSubtracao)          ,TmInt(-1);
  TmOp(TmInt 2,TmInt 2 ,OpMultiplicacao)      ,TmInt(4);
  TmOp(TmInt 4,TmInt 2 ,OpDivisao)            ,TmInt(2);
  TmOp(TmInt 1,TmInt 2 ,OpMenor)              ,TmBool(true);
  TmOp(TmInt 1,TmInt 2 ,OpMenorOuIgual)       ,TmBool(true);
  TmOp(TmInt 1,TmInt 2 ,OpIgual)              ,TmBool(false);
  TmOp(TmInt 2,TmInt 2 ,OpIgual)              ,TmBool(true);
  TmOp(TmInt 1,TmInt 2 ,OpDiferente)          ,TmBool(true);
  TmOp(TmInt 1,TmInt 1 ,OpDiferente)          ,TmBool(false);
  TmOp(TmInt 1,TmInt 2 ,OpMaiorOuIgual)       ,TmBool(false);
  TmOp(TmInt 1,TmInt 2 ,OpMaior)              ,TmBool(false);
  TmIf( TmBool(true),TmOp(TmInt 1,TmInt 2 ,OpSubtracao),TmInt(0) ),TmInt(-1);
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