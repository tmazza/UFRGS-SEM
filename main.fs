(* Gramatica: t ::= true | false | if (t1,t2,t3) | 0 | succ(t) | pred(t) | iszero (t) *)
type term =
 | TmTrue
 | TmFalse
 | TmIf       of term * term * term
 | TmZero
 | TmSucc     of term
 | TmPred     of term
 | TmIsZero   of term

(* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
// TODO: definir uma execeção
exception NoRuleApplies

(* Funcao auxiliar para determinar se um termo e um VALOR NUMERICO *)
let rec isnumerical t = 
  match t with
    | TmZero        -> true
    | TmSucc (t1)   -> isnumerical t1
    | _             -> false

(* Implementacao da funcao STEP de avaliacao em um passo *)
let rec step t = 
  match t with
    
    (* If *)
    | TmIf( TmTrue, t2, t3 )    -> t2                                     // E−IfTrue
    | TmIf( TmFalse, t2, t3 )   -> t3                                     // E−IfFalse
    | TmIf( t1, t2, t3 )        -> let t1' = step t1 in TmIf( t1',t2,t3 ) // E-if
    
    (* Succ *)
    | TmSucc( t1 )              -> let t1' = step t1 in TmSucc( t1' )     // E−Succ
    
    (* Pred *)
    | TmPred( TmZero )          -> TmZero                                 // E-PredZero
    | TmPred( TmSucc(nv) )                                                // E−PredSucc
        when (isnumerical nv)   -> nv                                     
    | TmPred( t1 )              -> let t1' = step t1 in TmPred( t1' )     // E-Pred

    (* isZero() *)
    | TmIsZero( TmZero )        -> TmTrue                                 // E-IsZeroZero
    | TmIsZero( TmSucc(nv) ) 
        when (isnumerical nv)   -> TmFalse                                // E−IsZeroSucc
    | TmIsZero( t1 )            -> let t1' = step t1 in TmIsZero( t1' )   // E−IsZero

    (* nenhuma regra se aplica *)
    | _                         -> raise NoRuleApplies

(* Implementacao de EVAL *)
let rec eval t =
  try let t' = step t
      in eval t'
  with NoRuleApplies -> t


(*TESTES*)
let evalList list =
  List.map (fun x-> (fst x,eval (fst x)),(snd x) ) list

let testes = [
  TmIsZero (TmZero),TmTrue;
  TmIsZero (TmSucc(TmZero)),TmFalse;
  TmIsZero (TmPred(TmSucc(TmZero))),TmTrue;
  TmIf(TmTrue,TmZero,TmTrue),TmZero;
  TmTrue,TmTrue;
  TmFalse,TmFalse;
  TmIsZero (TmPred( TmPred(TmSucc(TmZero)))),TmTrue;
  TmIf(TmTrue,TmIf(TmFalse,TmZero,TmSucc(TmZero)),TmTrue),TmSucc(TmZero);
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