 " Vim syntax file
 " Language: Nova
 " Maintainer: Carl Edlund
 " Latest revision: 23 June 2020

if exists ("b:current_syntax")
        finish
endif

syn match operator '[!"@$%&/=?+\\^~*'-]\+'
syn match operator '[:;<>|]\+'
syn match operator '`[a-zA-Z0-9]*`'
syn match loop '@[a-z0-9][a-zA-Z0-9]*'
syn match name '\*\?[0-9]*[a-z][a-zA-Z0-9]*\*\?'
syn match name '`[!$%&/=?+\\^~*-]\+`'
syn match name '`[:<>|]\+`'
syn match num '-\?[0-9]\+\>'
syn match num '-\?[0-9]\+\.[0-9]\+'
syn match cap '[A-Z][a-zA-Z0-9]*'
syn match cap '[0-9]\+[A-Z][a-zA-Z0-9]*'
syn region string start='"' end='"'
syn match comment '--.*$'
syn match char '\\\(nul\|space\|newline\|tab\|[^ \n\t]\)'
syn keyword keywords as case class destroy do each enum for from if infixl infixr lambda let local match modify parallel pure range rec seq static struct switch synonym tcase the type union use while
syn region comment start='\*\*' end='\*\*'

hi def link error       Error
hi def link name        Debug
hi def link keywords    Macro
hi def link cap         Type
hi def link num         Constant
hi def link comment     Comment
hi def link operator    Operator
hi def link string      Constant
hi def link loop        Macro
hi def link char        Constant
hi def link access      Debug
