 " Vim syntax file
 " Language: Nova
 " Maintainer: Carl Edlund
 " Latest revision: 23 June 2020

if exists ("b:current_syntax")
        finish
endif

syn match loop '@[a-z0-9]*'
syn match cap '[A-Z][a-zA-Z0-9]*'
syn match operator '[!$%&/=?+\\^~*-]\+'
syn match operator '[:;<>|]\+'
syn match operator '`[a-zA-Z0-9]*`'
syn match name '[a-z0-9_][a-zA-Z0-9_]*'
syn match num '-\?[0-9]\+\>'
syn match num '-\?[0-9]\+\.[0-9]\+'
syn match comment '--.*$'
syn match string '".*"'
syn match char '\'\(nul\|space\|newline\|tab\|.\)'
syn keyword keywords struct type union synonym switch tcase as the case from range lambda action modify
syn match keywords '>>'
syn match keywords '\.\.\.'
syn match keywords '->'
syn match keywords ' = '
syn match keywords '\\'
syn match access '[a-z0-9_][a-zA-Z0-9_]*\(\.[a-z0-9_][a-zA-Z0-9_]*\)\+'
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
