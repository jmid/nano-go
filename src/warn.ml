module LA = Last

type warning = Lexing.position * string

let unreachable pos (en,_) =
  let (sigma,f) = Analyzer.Intanalyzer.Cache.lookup en pos.LA.label in
  if Intstore.leq sigma Intstore.bot
  then [ (pos.LA.lex_pos, "Line unreachable") ]
  else []

let communication pos bodylab msg (en,_) =
  let (sigma,_) = Analyzer.Intanalyzer.Cache.lookup en pos.LA.label in
  let (sigma',_) = Analyzer.Intanalyzer.Cache.lookup en bodylab in
  if not (Intstore.leq sigma Intstore.bot) (* comm is reachable *)
    && (Intstore.leq sigma' Intstore.bot)  (* but body is not   *)
  then [ (pos.LA.lex_pos, msg) ]
  else []
    
(*  warn_stmt : Last.stmt -> Cache * Cache -> warning list *)
let rec warn_stmt s ((en,ex) as caches) = match s with
  | LA.Skip   pos       -> [] (* don't warn about unreachable skips (internal artifact) *)
  | LA.Assign (pos,_,_) -> unreachable pos caches
  | LA.Seq (s1,s2) ->
    let ws1 = warn_stmt s1 caches in
    let ws2 = warn_stmt s2 caches in
    ws1@ws2
  | LA.For (pos,_,s') ->
    let w = unreachable pos caches in
    let ws = warn_stmt s' caches in
    w@ws
  | LA.If (pos,_,s1,s2) ->
    let w = unreachable pos caches in
    let ws1 = warn_stmt s1 caches in
    let ws2 = warn_stmt s2 caches in
    w@ws1@ws2
  | LA.Select (pos,cs) ->
    let w = unreachable pos caches in
    w @ (List.concat
           (List.map (fun c -> warn_case c caches) cs))
  | LA.Print (pos,_) -> unreachable pos caches

and warn_case (pos,c) ((en,ex) as caches) = match c with
  | LA.ReadCase (x,ch,s) ->
    let ws = warn_stmt s caches in
    let w = communication pos (LA.first s) "Read cannot succeed" caches in
    w@ws
  | LA.WriteCase (ch,e,s) ->
    let ws = warn_stmt s caches in
    let w = communication pos (LA.first s) "Write cannot succeed" caches in
    w@ws

(*  warn_prog : Last.prog -> Cache * Cache -> warning list  *)
let warn_prog (chs,ps) =
  let ws =
    List.concat
      (List.map (fun (xs,s,caches) -> warn_stmt s caches) ps) in
  List.sort_uniq Pervasives.compare ws
