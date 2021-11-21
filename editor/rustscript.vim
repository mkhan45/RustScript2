" adapted from
" https://github.com/thesephist/ink/blob/7d47f9f39c539381085ff2dd07ce5b356916daa9/utils/ink.vim, 
" https://vim.fandom.com/wiki/Creating_your_own_syntax_files

if exists("b:rustscript_syntax")
    finish
endif

syntax sync fromstart

" prefer hard tabs
set noexpandtab

" keywords
syn keyword rscKeyword match let when
syn keyword rscKeyword if then else
syn keyword rscKeyword for in
highlight link rscKeyword Keyword

" operators
syntax match rscOp "+"
syntax match rscOp "-"
syntax match rscOp "*"
syntax match rscOp "/"
syntax match rscOp "^"
syntax match rscOp "$"
syntax match rscOp "%"
syntax match rscOp "&&"
syntax match rscOp "||"
syntax match rscOp "<="
syntax match rscOp "<"
syntax match rscOp ">"
syntax match rscOp ">="
syntax match rscOp "mod"
highlight link rscOp Operator

syntax match rscMatchArrow "->" 
highlight link rscMatchArrow Label

syntax match rscFnArrow "\v\=\>" 
syn keyword rscFnKeyword fn
highlight link rscFnArrow Function
highlight link rscFnKeyword Function

syntax match rscNumber "\v\d+"
syntax match rscNumber "\v\d+\.\d+"
highlight link rscNumber Number

syntax match rscBool "T"
syntax match rscBool "F"
highlight link rscBool Boolean

syntax match rscAtom "\v:\[A-za-z][A-za-z0-9_]+"
highlight link rscAtom Constant

syntax match rscIdentifier "\v[A-Za-z@!?][A-Za-z0-9@!?]*"
syntax match rscIdentifier "\v_"
highlight link rscIdentifier Identifier

syntax region rscString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link rscString String

" comments
syntax match rscComment "#.*"
highlight link rscComment Comment
