
module Shell = Ocamlbuild_pack.Shell
open Ocamlbuild_plugin

let (@*) l1 l2 =
  List.map (fun (a,b) -> a^b) @@ List.flatten @@ List.map (fun n1 -> List.map (fun n2 -> (n1,n2)) l2) l1

let (/+) p l =
  List.map (fun n -> p / n) l

let (/*) p () =
  p /+ (Array.to_list @@ Sys.readdir @@ (Filename.current_dir_name / p))

let (^+) l e =
  List.map (fun n -> n ^ e) l

let (|+) s l =
  s ^ "<{" ^ (String.concat "," l) ^ "}>"

let (-.+) l e =
  List.map (fun n -> n -.- e) l

let prog = "native"

(* paths *)

let tools = "tools"
let unidata = "unidata"
let mappings = "mappings"
let charmaps = "charmaps"
let database = "database"

(* the project generates many different '.mar' files using several
   different build flows: is there a way to tag target filenames
   so that they'd use a specific rule?
   For now, each category of files get its own static, custom set of
   rules.
*)



module MkDirMake (D : sig val path : string end) = struct

  let compiler = "/bin/mkdir"
  let gen env builder =
    Cmd (S [
      A compiler;
      Sh "-p";
      A D.path
    ])
  let rules () =
    rule ("mkdir "^D.path)
      ~prod:D.path
      ~deps:[compiler]
      gen

end

(*
module MkDir = struct

  let compiler = "/bin/mkdir"

  let gen env builder =
    Cmd (S [
      A compiler;
      Sh "-p";
      P (env "%(path)")
    ])

  let rules () =
    rule "%(path)"
      ~prod:"%(path)"
      ~deps:[]
      gen

end    
*)

module Unidata = struct

  let compiler = tools / "parse_unidata" -.- prog

  module Input = struct
    let path = unidata
    let dep = path / "UnicodeData.txt"
  end

  module Output = struct

    let path = database
    let filenames = [
      "combined_class";
      "composition";
      "decomposition";
      "general_category";
      "to_lower1";
      "to_title1";
      "to_upper1";
      "general_category_map"
    ]
    let prods = path /+  filenames -.+ "mar"

    let prod = path / ( "%(filename:" |+ filenames ) ^ ")" -.- "mar"

  end

  let gen env builder =
    Seq [
      Cmd (S [
	A "/bin/mkdir";
	Sh "-p";
	A database
      ]);
      Cmd (S [
	A compiler;
	A Output.path;
	Sh "<";
	P Input.dep
      ])
    ]

  let rules () =
    rule (Input.dep ^ " -> " ^ Output.prod)
      ~prod:Output.prod
      ~deps:[compiler;Input.dep]
      gen

end


module Props = struct

  let proplist = "PropList.txt"

  let filenames = [
    "White_Space";
    "Bidi_Control";
    "Join_Control";
    "Hyphen";
    "Quotation_Mark";
    "Terminal_Punctuation";
    "Other_Math";
    "Hex_Digit";
    "ASCII_Hex_Digit";
    "Other_Alphabetic";
    "Ideographic";
    "Diacritic";
    "Extender";
    "Other_Lowercase";
    "Other_Uppercase";
    "Noncharacter_Code_Point";
    "Other_Grapheme_Extend";
    "Grapheme_Link";
    "IDS_Binary_Operator";
    "IDS_Trinary_Operator";
    "Radical";
    "Unified_Ideograph";
    "Other_Default_Ignorable_Code_Point";
    "Deprecated";
    "Soft_Dotted";
    "Logical_Order_Exception"
  ]
end

module CoreDerivedProps = struct

  let proplist = "DerivedCoreProperties.txt"

  let filenames = [
    "Math";
    "Alphabetic";
    "Lowercase";
    "Uppercase";
    "ID_Start";
    "ID_Continue";
    "XID_Start";
    "XID_Continue";
    "Default_Ignorable_Code_Point";
    "Grapheme_Extend";
    "Grapheme_Base"
  ]

end

module PropsTxt(F : sig val proplist : string val filenames : string list end) = struct

  let compiler = "/bin/grep"

  module Input = struct
    let path = unidata
    let file = path / F.proplist
  end

  module Output = struct
    let path  = unidata
    let prod = path / ("%(filename:" |+ F.filenames) ^ ")"  -.- "txt"
  end

  let gen env builder =
    Cmd (S [
      A compiler;
      A "-F";
      P (env "%(filename)");
      P (Input.file);
      Sh ">";
      P (env @@ Output.path / "%(filename).txt")
    ])

  let rules () =
      rule (Input.file ^" -> " ^ Output.path ^ "/%(filename).txt")
      ~prod:Output.prod
      ~dep:Input.file
      gen

end

module Uniset = struct

  let compiler = tools / "parse_uniset" -.- prog

  let gen dep prop out env builder =
    Cmd (S [
      A compiler;
      A out;
      P prop;
      Sh "<";
      P dep;
    ])

end


module PropMake (F : sig val proplist : string val filenames : string list end) = struct

  include Uniset

  module Tbls = PropsTxt(F)

  module Input = struct
    let path = unidata
    let dep = path / "%(filename).txt"
  end

  module Output = struct
    let path = database
    let prod = path / "%(property)(_set)?.mar"
    let prods = path /+ (F.filenames @* ["";"_set"]) -.+ "mar"
  end

  (* make a special rule for each couple of '%(property)(_set)?.mar' files *)
  let rules () =
    Tbls.rules ();
    List. iter (fun prop ->
      let f1 = Output.path / prop -.- "mar"
      and f2 = Output.path / prop ^ "_set.mar"
      and dep = Input.path / prop -.- "txt" in
      rule (dep ^" -> "^ Output.prod)
	~prods:[f1;f2]
	~deps:[compiler;dep]
	(gen dep prop Output.path)
    ) F.filenames

end

module PropTbls = PropMake(Props)
module CoreDerivedPropTbls = PropMake(CoreDerivedProps)

module StringPrepTables = struct

  (* program used to produce the mapping files *)
  let compiler = tools / "camomilestringprep" -.- prog

  module Input = struct
    let path = unidata / "stringprep"
    let filenames = path /* ()
    let preds = filenames
  end


  module Output = struct
    let path = database
    let filenames = [
      "map_b1b2";
      "map_b1";
      "d1";
      "d2";
      "saslprep_map";
      "nodeprep_prohibited";
      "resourceprep_prohibited";
      "nameprep_prohibited";
      "saslprep_prohibited";
      "trace_prohibited";
      "iscsi_prohibited";
      "mib_prohibited"
    ]
    let prod = path / ("%(filename:" |+ filenames) ^ ")"  -.- "mar"
  end

  (* command to produce the mapping files *)
  let gen env builder =
    Cmd (S [
      A compiler;
      S [
	A "-in";
	P Input.path;
      ];
      S [
	A "-out";
	P Output.path;
      ]
    ])

  let rules () =
    rule ( Input.path ^ " -> " ^ Output.prod)
      ~prod:Output.prod
      ~deps:(compiler::Input.preds)
      gen

end

module AllKeys = struct

  module Input = struct
    let path = unidata / "tr10"
    let dep = path / "allkeys.txt"
  end

  module Output = struct
    let path = database
    let prod = path / "allkeys.mar"
  end

  let compiler = tools / "parse_allkeys" -.- prog

  let gen env builder =
    Cmd (S [
      A compiler;
      A Output.path;
      Sh "<";
      A Input.dep
    ])

  let rules () =
    rule (Input.dep ^ " -> " ^ Output.prod)
      ~prod:Output.prod
      ~deps:([compiler;Input.dep] @ PropTbls.Output.prods @ Unidata.Output.prods)
      gen

end

module DedicatedCompiler(F : sig
  val dep : string
  val prod : string
  val compiler : string
end) = struct

  module Input = struct
    let dep = F.dep
  end
  module Output = struct
    let prod = F.prod
  end

  let gen env builder =
    Cmd (S [
      A F.compiler;
      A database;
      Sh "<";
      A Input.dep
    ])

  let rules () =
    rule (Input.dep ^ " -> " ^ Output.prod)
      ~prod:Output.prod
      ~deps:[F.compiler;Input.dep]
      gen
end

module CaseFolding = DedicatedCompiler(
struct
  let dep = unidata / "CaseFolding.txt"
  let prod = database / "case_folding.mar"
  let compiler = tools / "parse_casefolding" -.- prog
end)

module CompositionExclusion = struct
  include Uniset
  let dep = unidata / "CompositionExclusions.txt"
  let filename = "composition_exclusion"
  let prod = database / filename -.- "mar"

  let rules () =
    rule (dep ^ " -> " ^ prod)
      ~prod
      ~deps:[compiler;dep]
      (gen dep filename database)
end

module SpecialCasing = DedicatedCompiler(struct
  let dep = unidata / "SpecialCasing.txt"
  let prod = database / "special_casing.mar"
  let compiler = tools / "parse_specialcasing" -.- prog
end)

module Scripts = DedicatedCompiler(struct
  let dep = unidata / "Scripts.txt"
  let prod = database / "scripts.mar"
  let compiler = tools / "parse_scripts" -.- prog
end)

module Age = DedicatedCompiler(struct
  let dep = unidata / "DerivedAge.txt"
  let prod = database / "age.mar"
  let compiler = tools / "parse_age" -.- prog
end)


module Mappings = struct

  (* all mapping files are generated in one go by mappings/gen_mapping.ml
     => could we define a dummy target, which will cover all of them
     without having to name them?
     Q: How do we proceed with oasis?
  *)

  (* program used to produce the mapping files *)
  let compiler = mappings / "gen_mappings" -.- prog

  (* command to produce the mapping files *)
  let gen env builder =
    Cmd (S [
      A compiler
    ])

  (* input and output are hardcoded in the program *)
  module Input = struct
    let path = charmaps
    let filenames = [
      "GB2312";
      "ISO-8859-7";
      "EUC-JP";
      "EUC-KR";
      "EUC-TW"
    ]
    let dep_label = path / ("%(filename:" |+ filenames) ^ ")"
    let deps = path /+ filenames
  end

  module Output = struct
    let path = mappings
    let filenames = [
      "cns11643";
      "jisx0201";
      "ksc5601";
      "gb2312";
      "jisx0208";
      "iso88597";
      "jisx0212"
    ]
    let prod = path / ("%(filename:" |+ filenames) ^ ")"  -.- "mar"
  end

  let rules () =
    rule (Input.dep_label ^ " -> " ^ Output.prod)
      ~prod:Output.prod
      ~deps:(compiler::Input.deps)
      gen

end

module CharMaps = struct
  (* charmaps files are generated on demand by tools/camomilecharmap.ml
     we may define an extension based rule, and specify each target
     individually.
  *)

  module Input = struct
    let path = charmaps
    (* dependencies : how do I tell ocamlbuild to include the whole dir? *)
    let deps = path /* ()
  end

  module Output = struct
    let path = Input.path
  end

  let compiler = tools / "camomilecharmap" -.- prog

  let gen env builder = Cmd (S [
    A compiler;
    S [ A "-d";
	P Output.path ];
    P (env @@ charmaps / "%(filename)")
  ])

  let rules () =
    rule (Input.path ^"/%(filename) -> "^Output.path^"/%(filename).mar")
      ~prod:(Output.path / "%(filename).mar")
      ~deps:(compiler::Input.deps)
      gen

end

module Locales = struct
  (* locales may be generated individually or in batch from the command line
  *)

  module Input = struct
    let path = "locales"
    (* dependencies : how do I tell ocamlbuild to include the whole dir? *)
    let deps = path /* ()
  end

  module Output = struct
    let path = Input.path
  end

  let compiler = tools / "camomilelocaledef" -.- prog

  let gen env builder = Cmd (S [
    A compiler;
    S [ A "--file";
	P (Input.path / env "%(filename).txt") ];
    A Output.path
  ])

  let rules () =
    rule (Input.path ^"/%(filename).txt -> "^Output.path^"/%(filename).mar")
      ~prod:(Output.path / "%(filename).mar")
      ~deps:(compiler::Input.deps)
      gen

end

let _ = dispatch begin function
  | After_rules -> List.iter (fun f -> f ()) [
(*    MkDir.rules; *)
    Unidata.rules;
    PropTbls.rules;
    CoreDerivedPropTbls.rules;
    StringPrepTables.rules;
    Mappings.rules;
    CharMaps.rules;
    AllKeys.rules;
    CaseFolding.rules;
    CompositionExclusion.rules;
    SpecialCasing.rules;
    Scripts.rules;
    Age.rules;
    Locales.rules;
  ]
  | _ -> ()
end
