namespace Interpretador

module Gramatica =

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
    | TmNum of int                      // n
    | TmBool of bool                    // b
    | TmOp of term * term * Operacao    // e1 op e2
    | TmIf of term * term * term        // if e1 then e2 else e3
    | TmIdent of string                 // x
    | TmApp of term * term              // e1 e2  
    | TmFunc of string * term           // fn x:T => e
    | TmLet of string * term * term     // let x = e1 in e2
    | TmLetRec of string * term * term  // let rec f:T1->T2 = e1 in e2
    (* Listas *)
    | TmNil                             // []     | TODO: step
    | TmCons of term * term             // e1::e2 | TODO: step
    | TmIsEmp of term                   // isempty e1  | TODO: step
    | TmHd of term                      // hd e1  | TODO: step
    | TmTl of term                      // tl e1  | TODO: step
    (* Exceções *)
    | TmRaise                           // raise
    | TmTry of term * term              // try e1 with e2

  (* Excecao a ser ativada quando termo for uma FORMA NORMAL *)
  exception NoRuleApplies

module Avaliador =
  
  open Gramatica

  (* Funcao auxiliar para determinar se um termo e um valor numérico *)
  let isNum t = 
    match t with 
    | TmNum(n)  -> true 
    | _ -> false

  (* Se termo é ou não um valor *)
  let isValue t = 
    match t with
    | TmNum(n)  -> true
    | TmBool(n) -> true
    | TmRaise   -> true
    | _ -> false

  let toInt n =
    match n with
    | TmNum(n) -> n
    | _        -> 0

  let toBool n =
    match n with
    | TmBool(b) -> b
    | _         -> false

  // { v/x } e
  let rec subFree v x e =
    match e with 
    | TmIf( e1,e2,e3 ) -> TmIf(subFree v x e1,subFree v x e2,subFree v x e3)
    | TmOp( e1,e2,op ) -> TmOp(subFree v x e1,subFree v x e2, op)
    | TmApp( e1,e2 ) -> TmApp(subFree v x e1,subFree v x e2)
    | TmFunc( y,e1 ) when not(x = (TmIdent y)) -> TmFunc( y,subFree v x e1)
    | TmLet( y,e1,e2 ) when x = (TmIdent y) 
        -> TmLet( y,subFree v x e1, e2)
    | TmLet( y,e1,e2 ) when not(x = (TmIdent y)) 
        -> TmLet( y,subFree v x e1, subFree v x e2)
    | TmLetRec( f, TmFunc( y, e1), e2) when not(x = TmIdent f) 
        -> TmLetRec( f, subFree v x (TmFunc( y, e1)), subFree v x e2)
    | _ -> if x = e then v else e

  (* Implementacao da funcao STEP de avaliacao em um passo *)
  let rec step t = 
    match t with
      (* Op *)
      | TmOp( TmRaise,e2,op ) -> TmRaise
      | TmOp( e1,TmRaise,op ) -> TmRaise
      | TmOp( e1,e2,op ) 
          when isNum e1 && isNum e2 ->
            match op with
            | OpSo  -> TmNum( (toInt e1) + (toInt e2) )
            | OpSu  -> TmNum( (toInt e1) - (toInt e2) )
            | OpMu  -> TmNum( (toInt e1) * (toInt e2) )
            | OpDi  -> TmNum( (toInt e1) / (toInt e2) )
            | OpDi when (toInt e2 = 0) -> TmRaise
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
      | TmIf( TmBool(true),e2,e3 ) -> e2
      | TmIf( TmBool(false),e2,e3 ) -> e3
      | TmIf( TmRaise,e2,e3 ) -> TmRaise
      | TmIf( e1, e2, e3 ) -> 
          let e1' = step e1 in TmIf( e1',e2,e3 )
      
      (* App *)
      | TmApp( TmFunc(x,e),v ) -> subFree v (TmIdent x) e
      | TmApp( TmRaise,e2 ) -> TmRaise
      | TmApp( e1,TmRaise ) when isValue e1 -> TmRaise
      | TmApp( e1,e2 ) when isValue e1 ->
          let e2' = step e2 in TmApp( e1,e2' )
      | TmApp( e1,e2 ) ->
          let e1' = step e1 in TmApp( e1',e2 )
      
      (* Let *)
      | TmLet( x,e1,e2 ) when isValue e1 -> subFree e1 (TmIdent x) e2
      | TmLet( x,e1,TmRaise ) -> TmRaise
      | TmLet( x,e1,e2 ) ->
          let e1' = step e1 in TmLet( x,e1',e2 )

      (* Let rec *)
      | TmLetRec( f,TmFunc( y, e1),TmRaise ) -> TmRaise
      | TmLetRec( f,TmFunc( y, e1),e2) 
          -> subFree (TmFunc( y, TmLetRec( f,TmFunc( y,e1 ),e1 ) )) (TmIdent f) e2

      (* try-with *)
      | TmTry( TmRaise,e2 ) -> e2
      | TmTry( e1,e2 ) when isValue e1 -> e1
      | TmTry( e1,e2 ) ->
          let e1' = step e1 in TmTry( e1',e2 )

      (* NoRuleApplies *)
      | _ -> raise NoRuleApplies

  (* Implementacao de eval *)
  let rec eval t =
    try let t' = step t
        in eval t'
    with NoRuleApplies -> t

(* TESTES *)
module Testes =

  open Gramatica

  let evalList list =
    List.map (fun x-> (fst x,Avaliador.eval (fst x)),(snd x) ) list

  // Lista de pares (entrada,saida esperada)
  let testes = [
    (* TESTES STEP *)
    
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
    
    // let rec simple = fn x => x in simple(1)
    TmLetRec("simple", TmFunc( "x", TmIdent("x") ),TmApp(TmIdent("simple"),TmNum(1))),TmNum(1);

    // let rec fat = fn x => if x=0 then 1 else x * fat(x-1) in fat(5) // fatorial
    TmLetRec("fat", TmFunc( "x", TmIf(
                                TmOp( TmIdent("x"),TmNum(0),OpIg ),
                                TmNum(1),
                                TmOp( 
                                  TmIdent("x"),
                                  TmApp( TmIdent("fat"),TmOp( TmIdent("x"),TmNum(1),OpSu ) ),
                                  OpMu 
                                )
    )),TmApp(TmIdent("fat"),TmNum(5))),TmNum(120);

    // raise + 1 -> raise
    TmOp( TmRaise,TmNum(1),OpSo ),TmRaise;

    // 1 + raise -> raise
    TmOp( TmNum(1),TmRaise,OpSo ),TmRaise;

    // if raise then 1 else 2 -> raise
    TmIf( TmRaise,TmNum(1),TmNum(2) ),TmRaise;

    // (fn x:T => x) (raise) -> (raise)
    TmApp( TmFunc("x",TmIdent("x")),TmRaise ),TmRaise;

    // (raise) (2) -> (raise)
    TmApp( TmRaise, TmNum(2) ),TmRaise;

    // (let x = 1 in raise) -> raise
    TmLet("x",TmNum(1), TmRaise),TmRaise;
    
    // (let x = raise in 1) -> raise // x não ocorre em e1
    TmLet("x",TmRaise, TmNum(1)),TmNum(1);

    // (let x = raise in x) -> raise // x ocorre em e1
    TmLet("x",TmRaise, TmIdent("x")),TmRaise;

    // let rec simple = fn x => x in raise
    TmLetRec("simple", TmFunc( "x", TmIdent("x") ),TmRaise),TmRaise;

    // try if true then 1 else 2 with 5 -> 1
    TmTry( TmIf( TmBool(true),TmNum(1),TmNum(2) ),TmNum(5) ),TmNum(1);
    
    // try if true then raise else 2 with 5 -> 5
    TmTry( TmIf( TmBool(true),TmRaise,TmNum(2) ),TmNum(5) ),TmNum(5);

    // try 1 else 2 with 5 -> 1
    TmTry( TmNum(1),TmNum(5) ),TmNum(1);

    // try 1 else 2 with raise -> 1
    TmTry( TmNum(1),TmRaise ),TmNum(1);

    (******* Testes substituição (subFree) *******) // TODO: separar os testes de substituição
    (** Teste: {e/x}n -> n 
      (fn x:T => 1) (2) **)
    TmApp( TmFunc("x", TmNum(1)), TmNum(2)),TmNum(1);
    
    (** Teste: {e/x}b -> b
      (fn x:T => True) (2) **)
    TmApp( TmFunc("x", TmBool(true)), TmNum(2)),TmBool(true);
    
    (** Teste: {e/x}(e1 op e2) -> ({e/x}e1 op {e/x}e2)
      (fn x:T => x + x) (2) **)
    TmApp( TmFunc("x", TmOp(TmIdent("x"),TmIdent("x"),OpSo) ), TmNum(2)),TmNum(4);
    
    (** Teste: {e/x}(if e1 then e2 else e3) -> (if {e/x}e1 then {e/x}e2 else {e/x}e3)
      (fn x:T => if x > 0 then x - 1 else x + 1 ) (2) **)
    TmApp( TmFunc("x", TmIf(TmOp(TmIdent("x"),TmNum(0),OpMa),
                            TmOp(TmIdent("x"),TmNum(1),OpSu),
                            TmOp(TmIdent("x"),TmNum(1),OpSo)
    )), TmNum(2)),TmNum(1);
    
    (** Teste: {e/x}(e1 e2) -> ({e/x}e1 {e/x}e2)
      (fn x:T => (x (x-1)) ) (2) **)
    TmApp( TmFunc("x", TmApp(
                          TmIdent("x"),
                          TmOp(TmIdent("x"),TmNum(1),OpSu)
    )), TmNum(2)),TmApp(TmNum(2),TmNum(1));
    
    (** Teste: {e/x}x -> e
      (fn x:T => x) (2) **)
    TmApp( TmFunc("x", TmIdent("x")), TmNum(2)),TmNum(2);
    
    (** Teste: {e/x}y -> y
      (fn x:T => y) (2) **)
    TmApp( TmFunc("x", TmIdent("y")), TmNum(2)),TmIdent("y");
    
    (** Teste: {e/x} fn x:T => e' -> {e/x} fn x:T => e' // variável pressa, substituição não é aplica
      (fn x:T => (fn x:T => x)) (2) **)
    TmApp( TmFunc("x", TmFunc("x", TmIdent("x"))), TmNum(2)),TmFunc("x", TmIdent("x"));
    
    (** Teste: {e/x} fn y:T => e' -> {e/x} fn y:T => {e/x}e'
      (fn x:T => (fn x:T => y + x)) (2) **)
    TmApp( TmFunc("x", TmFunc("y",
                          TmOp(TmIdent("y"),TmIdent("x"),OpSo)
    )), TmNum(2)),TmFunc("y",TmOp(TmIdent("y"),TmNum(2),OpSo));
    
    (** Teste: {e/x}(let x:T => e1 in e2) -> let x:T {e/x}e1 in e2
      (fn x:T => (let x:T => x + 3 in x)) (2) **) // Valor do x em e2 só é procesado após substituição do x em e1
    TmApp( TmFunc("x", TmLet("x",TmOp(TmIdent("x"),TmNum(3),OpSo),TmIdent("x"))), TmNum(2)),TmNum(5);
    
    (** Teste: {e/x}(let y:T => e1 in e2) -> let y:T {e/x}e1 in {e/x}e2
      (fn x:T => (let y:T => x + 3 in x)) (2) **) // x é livre em e2
    TmApp( TmFunc("x", TmLet("y",TmOp(TmIdent("x"),TmNum(3),OpSo),TmIdent("x"))), TmNum(2)),TmNum(2);

    (** Teste: {e/f}(let rec f:T->T=(fn y:T => e1) in e2) -> (let rec f:T->T=(fn y:T => e1) in e2) 
    (fn f:T => (let rec f = fn x => x) (2) **) // f presso, nada é alterado. Valor 2 é desconsiderado
    TmApp( TmFunc("f", 
              TmLetRec("f", TmFunc( "x", TmIdent("x") ),TmApp(TmIdent("f"),TmNum(1)))
    ), TmNum(2)),TmNum(1);

    (** Teste: {e/x}(let rec f:T->T=(fn y:T => e1) in e2 -> (let rec f:T->T={e/x}(fn y:T => e1) in {e/x}e2 
    let rec fat = fn y => if y-x=0 then 1 else y * fat(y-1) in fat(6-x) **) // aplica substituição quado x != f
    TmApp( TmFunc("x", 
            TmLetRec("f", TmFunc( "y", TmIf(
                                      TmOp( TmIdent("y"),TmNum(0),OpIg ),
                                      TmNum(1),
                                      TmOp( 
                                        TmIdent("y"),
                                        TmApp( TmIdent("f"),TmOp( TmIdent("y"),TmNum(1),OpSu ) ),
                                        OpMu 
                                      )
            )),TmApp(TmIdent("f"),TmOp( TmNum(6),TmIdent("x"),OpSu )))
    ), TmNum(1)),TmNum(120);
    
  ]
  
  let prettyPrint list =
    List.map (fun x->
      if (snd (fst x) = snd x) then 
        printf "\n\tpass | %A -> %A" (fst (fst x)) (snd (fst x))
      else 
        printf "\n***fail | test: %A | output: %A insted of %A " (fst (fst x)) (snd (fst x)) (snd x)
    ) list

  let r = prettyPrint (evalList testes)
  printf "\n\n"