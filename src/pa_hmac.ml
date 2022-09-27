
open Pcaml ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Pp_MLast ;

type paren_t = [ PAREN | BRACKET | BRACE ][@@deriving show;] ;

type token = (string * string)[@@deriving show;] ;

type tt = [
    MATCHED of paren_t and list tt
  | TOKEN of token
  | VAR of Ploc.t and option string and string
]
[@@deriving show;]
;

value start_paren = fun [
  PAREN -> ("","(")
| BRACKET -> ("","[")
| BRACE -> ("","{")
]
;

value end_paren = fun [
  PAREN -> ("",")")
| BRACKET -> ("","]")
| BRACE -> ("","}")
]
;

value rec flatten_tts l =
  List.concat_map flatten_tt_element l
and flatten_tt_element = fun [
      TOKEN t -> [t]
    | MATCHED ty l -> [start_paren ty]@(flatten_tts l)@[end_paren ty]
]
;

type rule_t = {
    matcher: list tt
  ; freshlist: list token
  ; expansion: list tt
  }
;

type macro_t = [
    DECLARATIVE of (list rule_t)
  | PROCEDURAL of (list tt) -> (list token)
  ]
;

value macros = ref [] ;

value recognize_freshvars l =
  l |> List.map (fun [ TOKEN(("LIDENT",_) as t) -> t
                     | t -> Fmt.(failwithf "recognize_freshvars: encountered non-LIDENT %a" pp_tt t) ])

;

value rec recognize_rules tts =
  match tts with [
      [(MATCHED _ mtts); TOKEN ("","=>"); (MATCHED _ etts); TOKEN ("",";") :: tl] ->
      [{matcher=mtts; freshlist=[]; expansion=etts} :: recognize_rules tl]
    | [(MATCHED _ mtts); TOKEN ("","=>"); (MATCHED _ etts)] ->
      [{matcher=mtts; freshlist=[]; expansion=etts}]
    | [(MATCHED _ mtts); TOKEN ("","=>"); (MATCHED _ ftts); (MATCHED _ etts); TOKEN ("",";") :: tl] ->
       let l = recognize_freshvars ftts in
      [{matcher=mtts; freshlist=l; expansion=etts} :: recognize_rules tl]
    | [(MATCHED _ mtts); TOKEN ("","=>"); (MATCHED _ ftts); (MATCHED _ etts)] ->
       let l = recognize_freshvars ftts in
      [{matcher=mtts; freshlist=l; expansion=etts}]
    | [] -> []
    ]
;

value recognize_macro_rules ptt =
  match ptt with [
      MATCHED _ tts ->
      let l = recognize_rules tts in
      l
    ]
;

value register_macro_rules name ptt =
  let r = recognize_macro_rules ptt in do {
    Std.push macros (name, DECLARATIVE r) ;
    ()
  }
 ;

value register_procedural_macro name f =
  Std.push macros (name, PROCEDURAL f) ;

value match_matcher mtts etts =
  ()
;

value execute_declarative_macro rl tts =
  assert False
;

value apply_macro e name tts =
  let l =
    match List.assoc name macros.val with [
        PROCEDURAL f ->
        f tts
      | DECLARATIVE rl -> execute_declarative_macro rl tts
      | exception Not_found ->
         Fmt.(failwithf "apply_macro: macro %s not found" name)
      ] in
  Grammar.Entry.parse_token_stream e (Stream.of_list l)
;

value echo_macro tts = flatten_tts tts ;

register_procedural_macro "ECHO" echo_macro ;

value is_not_endparen = fun [
  ("",s) -> s <> ")" &&  s <> "]" &&  s <> "}"
| (c,_) -> c <> ""
]
;

value is_not_paren = fun [
  ("",s) -> s <> "(" && s <> ")" &&  s <> "[" &&  s <> "]" &&  s <> "{" &&  s <> "}"
| (c,_) -> c <> ""
]
;

value is_not_eoi (c,_) = c <> "EOI" ;

value rec pa_tt_element = parser [
  [: `("ANTIQUOT_LOC",s) :] ->
  match Plexer.parse_antiloc s with [
      None -> Fmt.(failwithf "pa_tt_element: antiquotation %a was not parseable" Dump.string s)
    | Some (loc,  (""|"_"), v) -> VAR loc None v
    | Some (loc,  ty, v) -> VAR loc (Some ty) v
    ]
| [: `t when is_not_eoi t && is_not_paren t :] -> TOKEN t
| [: e = pa_paren_tt_element :] -> e
]
and pa_tt strm =
  let rec parec acc = parser [
    [: t = pa_tt_element ; strm :] -> parec [t :: acc] strm
  | [: :] -> List.rev acc
  ] in
  parec [] strm

and pa_paren_tt_element = parser [
  [: `("","(") ; l = pa_tt ; `("",")") :] -> MATCHED PAREN l
| [: `("","[") ; l = pa_tt ; `("","]") :] -> MATCHED BRACKET l
| [: `("","{") ; l = pa_tt ; `("","}") :] -> MATCHED BRACE l
]
;

value tt =
  Grammar.Entry.of_parser gram "tt"
    pa_tt
;

value paren_tt_element =
  Grammar.Entry.of_parser gram "paren_tt_element"
    pa_paren_tt_element
;

EXTEND
  GLOBAL: expr str_item ;

  expr: LEVEL "simple" [
    [ "MACRO_EXPR" ; name = UIDENT ; "!" ; body = paren_tt_element -> let s = "gargle" in <:expr< $str:s$ >>
    ]
  ]
  ;

  str_item: LEVEL "top" [
    [ "MACRO_RULES" ; "!" ; name = UIDENT ; body = paren_tt_element -> do {
        register_macro_rules name body ;
        <:str_item< declare end >>
      }
    | "MACRO_STR_ITEM" ; name = UIDENT ; "!" ; body = paren_tt_element ->
       apply_macro str_item name (match body with [ MATCHED _ l -> l ])
    ]
  ]
  ;
END;
