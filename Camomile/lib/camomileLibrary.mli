
module OOChannel : module type of OOChannel
module UChar : module type of UChar
module USet : module type of USet
module UMap : module type of UMap
module UCharTbl : module type of UCharTbl
module UnicodeString : module type of UnicodeString
module UText : module type of UText
module XString : module type of XString
module SubText : module type of SubText
module ULine : module type of ULine
module Locale : module type of Locale
module CharEncoding : module type of CharEncoding
module UTF8 : module type of UTF8
module UTF16 : module type of UTF16
module UCS4 : module type of UCS4
module UPervasives : module type of UPervasives
module URe : module type of URe
module UCharInfo : module type of UCharInfo

module type Type = sig

  module OOChannel : module type of OOChannel
  module UChar : module type of UChar
  module USet : module type of USet
  module UMap : module type of UMap
  module UCharTbl : module type of UCharTbl
  module UnicodeString : module type of UnicodeString
  module UText : module type of UText
  module XString : module type of XString
  module SubText : module type of SubText
  module ULine : module type of ULine
  module Locale : module type of Locale
  module CharEncoding : CharEncoding.Interface
  module UTF8 : module type of UTF8
  module UTF16 : module type of UTF16
  module UCS4 : module type of UCS4
  module UPervasives : module type of UPervasives
  module URe : module type of URe
  module UCharInfo : UCharInfo.Type
    
  module UNF : sig
    module type Type = UNF.Type
    module Make (Text : UnicodeString.Type) :
      Type with type text = Text.t and type index = Text.index
  end

  module UCol : sig
    (** How variables are handled *)
    type variable_option = 
      [ `Blanked 
      | `Non_ignorable 
      | `Shifted
      | `Shift_Trimmed ]
	
    (** Strength of comparison.  For European languages, each strength
	roughly means as
	`Primary : Ignore accents and case
	`Secondary : Ignore case but accents are counted in.
	`Tertiary : Accents and case are counted in.
	For the case of `Shifted, `Shift_Trimmed, there is the fourth strength.
	`Quaternary : Variables such as - (hyphen) are counted in. *)
    type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]

    module type Type = UCol.Type
    module Make (Text : UnicodeString.Type) :
      Type with type text = Text.t and type index = Text.index
  end

  module CaseMap : sig
    module type Type = CaseMap.Type
    module Make  (Text : UnicodeString.Type) : (Type with type text = Text.t)
  end

  module UReStr : UReStr.Interface

  module StringPrep : sig
    module type Type = StringPrep.Type
    module Make  (Text : UnicodeString.Type) : (Type with type text = Text.t)
  end

end


module Make (Config : ConfigInt.Type) : Type with
      module OOChannel = OOChannel and
      module UChar = UChar and
      module USet = USet and
      module UMap = UMap and
      module UCharTbl = UCharTbl and
      module UnicodeString = UnicodeString and
      module UText = UText and
      module XString = XString and
      module SubText = SubText and
      module ULine = ULine and
      module Locale = Locale and
      module CharEncoding = CharEncoding.Configure(Config) and
      module UTF8 = UTF8 and
      module UTF16 = UTF16 and
      module UCS4 = UCS4 and
      module UPervasives = UPervasives and
      module URe = URe and
      module UCharInfo = UCharInfo.Make(Config) and
      module UNF.Make = UNF.Make(Config) and
      module UCol.Make = UCol.Make(Config) and
      module CaseMap.Make = CaseMap.Make(Config) and
      module UReStr = UReStr.Configure(Config) and
      module StringPrep.Make = StringPrep.Make(Config)
