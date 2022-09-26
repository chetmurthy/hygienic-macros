
open Pcaml ;

type paren_t = [ PAREN | BRACKET | BRACE ] ;

type tt = [
    MATCHED of paren_t and list tt
  | TOKEN of Grammar.token
]
;

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
  [: `t when is_not_eoi t && is_not_paren t :] -> TOKEN t
| [: `("","(") ; l = pa_tt ; `("",")") :] -> MATCHED PAREN l
| [: `("","[") ; l = pa_tt ; `("","]") :] -> MATCHED BRACKET l
| [: `("","{") ; l = pa_tt ; `("","}") :] -> MATCHED BRACE l
]
and pa_tt strm =
  let rec parec acc = parser [
    [: t = pa_tt_element ; strm :] -> parec [t :: acc] strm
  | [: :] -> List.rev acc
  ] in
  parec [] strm
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

EXTEND
  GLOBAL: expr ;

  expr: LEVEL "simple" [
    [ "GARGLE" -> let s = "gargle" in <:expr< $str:s$ >>
    ]
  ]
  ;
END;
