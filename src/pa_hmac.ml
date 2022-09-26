
open Pcaml ;
open Pa_ppx_base ;
open Ppxutil ;

type paren_t = [ PAREN | BRACKET | BRACE ] ;

type tt = [
    MATCHED of paren_t and list tt
  | TOKEN of Grammar.token
  | VAR of Ploc.t and option string and string
]
;

value rec recognize_rules tts =
  match tts with [
      [(MATCHED _ _ as mtts); TOKEN ("","=>"); (MATCHED _ _ as etts); TOKEN ("",";") :: tl] ->
      [(mtts, etts) :: recognize_rules tl]
    | [(MATCHED _ _ as mtts); TOKEN ("","=>"); (MATCHED _ _ as etts)] ->
      [(mtts, etts)]
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
  let _ = recognize_macro_rules ptt in
  () ;

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

value rec flatten_tt l =
  List.concat_map flatten_tt_element l
and flatten_tt_element = fun [
      TOKEN t -> [t]
    | MATCHED ty l -> [start_paren ty]@(flatten_tt l)@[end_paren ty]
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
       <:str_item< declare end >>
    ]
  ]
  ;
END;
