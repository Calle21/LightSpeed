 " Vim syntax file
 " Language: Nova
 " Maintainer: Carl Edlund
 " Latest revision: 23 June 2020

if exists ("b:current_syntax")
        finish
endif

syn match cap '[A-Z][a-zA-Z0-9]*'
syn match loop '@'
syn match loop '@[a-z0-9][a-zA-Z0-9]*'
syn match operator '[!$%&/=?+\\^~*-]\+'
syn match operator '[:<>|]\+'
syn match operator '`[a-zA-Z0-9]*`'
syn match name '[a-z0-9_][a-zA-Z0-9_]*'
syn match name '`[!$%&/=?+\\^~*-]\+`'
syn match name '`[:<>|]\+`'
syn match name '\*[a-z0-9_][a-zA-Z0-9_]*'
syn match name '\*[a-z0-9_][a-zA-Z0-9_]*\*'
syn match num '-\?[0-9]\+\>'
syn match num '-\?[0-9]\+\.[0-9]\+'
syn region string start='"' end='"'
syn match comment '--.*$'
syn match char '\'\(nul\|space\|newline\|tab\|.\)'
syn keyword keywords as case class delay destroy do each enum for from if infixl infixr lambda let locals match modify parallel pure range seq static struct switch synonym tcase the type union use while
syn region comment start='\*\*' end='\*\*'


hi def link name            Debug
hi def link keywords        Macro
hi def link cap             Type
hi def link num             Constant
hi def link comment         Comment
hi def link operator        Operator
hi def link string          Constant
hi def link loop            Macro
hi def link char            Constant
hi def link access          Debug
