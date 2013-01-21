module Parser (
    parse,
) where

import State (Runtime, parseFail)
import Tokens
import Decls

parse :: [Decl] -> [Token] -> Runtime [Decl]
parse []              = shift []
parse (lastDecl:rest) = reduce ((demote lastDecl):rest)

shift :: [Decl] -> [Token] -> Runtime [Decl]
shift decls (tok:tokens) = reduce ((Dk tok):decls) tokens
shift decls []           = return decls

reduce :: [Decl] -> [Token] -> Runtime [Decl]

{- object -}
reduce decls@((Dk (STR _)):_)   tokens@(DOT:_) = shift decls tokens
reduce decls@((Dk (ID _)):_)    tokens@(DOT:_) = shift decls tokens
reduce decls@((Df (Fl _)):_)    tokens@(DOT:_) = shift decls tokens
reduce decls@((Dp _):_)         tokens@(DOT:_) = shift decls tokens
reduce decls@((Do _):_)         tokens@(DOT:_) = shift decls tokens
reduce decls@((De _):_)         tokens@(DOT:_) = shift decls tokens
reduce decls@((Dk DOT):(Dk (STR _)):_)  tokens = shift decls tokens
reduce decls@((Dk DOT):(Dk (ID _)):_)   tokens = shift decls tokens
reduce decls@((Dk DOT):(Df (Fl _)):_)   tokens = shift decls tokens
reduce decls@((Dk DOT):(Dp _):_)        tokens = shift decls tokens
reduce decls@((Dk DOT):(Do _):_)        tokens = shift decls tokens
reduce decls@((Dk DOT):(De _):_)        tokens = shift decls tokens
reduce ((Dk (ID prop)):(Dk DOT):(Dk (STR x)):rest) tokens = reduce ((Do (PropS x prop)):rest) tokens
reduce ((Dk (ID prop)):(Dk DOT):(Dk (ID x)):rest)  tokens = reduce ((Do (PropD x prop)):rest) tokens
reduce ((Dk (ID prop)):(Dk DOT):(Df (Fl x)):rest)  tokens = reduce ((Do (PropL x prop)):rest) tokens
reduce ((Dk (ID prop)):(Dk DOT):(Dp x):rest)       tokens = reduce ((Do (PropP x prop)):rest) tokens
reduce ((Dk (ID prop)):(Dk DOT):(Do x):rest)       tokens = reduce ((Do (PropO x prop)):rest) tokens
reduce ((Dk (ID prop)):(Dk DOT):(De x):rest)       tokens = reduce ((Do (PropE x prop)):rest) tokens

{- elem -}
reduce decls@((Dk (STR _)):_)           tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dk (ID _)):_)            tokens@(LBRA:_) = shift decls tokens
reduce decls@((Df (Fl _)):_)            tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dp _):_)                 tokens@(LBRA:_) = shift decls tokens
reduce decls@((Do _):_)                 tokens@(LBRA:_) = shift decls tokens
reduce decls@((De _):_)                 tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dc _):_)                 tokens@(LBRA:_) = shift decls tokens
reduce decls@((Dk LBRA):(Dk (STR _)):_)          tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dk (ID _)):_)           tokens = shift decls tokens
reduce decls@((Dk LBRA):(Df (Fl _)):_)           tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dp _):_)                tokens = shift decls tokens
reduce decls@((Dk LBRA):(Do _):_)                tokens = shift decls tokens
reduce decls@((Dk LBRA):(De _):_)                tokens = shift decls tokens
reduce decls@((Dk LBRA):(Dh _):_)                tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Dk (STR _)):_)   tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Dk (ID _)):_)    tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Df (Fl _)):_)    tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Dp _):_)         tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Do _):_)         tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(De _):_)         tokens = shift decls tokens
reduce decls@((Db _):(Dk LBRA):(Dh _):_)         tokens = shift decls tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dk (STR x)):rest) tokens = reduce ((De (ElemS x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dk (ID x)):rest)  tokens = reduce ((De (ElemD x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Df (Fl x)):rest)  tokens = reduce ((De (ElemL x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dp x):rest)       tokens = reduce ((De (ElemP x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Do x):rest)       tokens = reduce ((De (ElemO x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(De x):rest)       tokens = reduce ((De (ElemE x i)):rest) tokens
reduce ((Dk RBRA):(Db i):(Dk LBRA):(Dh x):rest)       tokens = reduce ((De (ElemC x i)):rest) tokens

{- list -}
reduce decls@((Dk LBRA):_)                            tokens = shift decls tokens
reduce ((Dk RBRA):(Dk LBRA):rest)                     tokens = reduce ((Df (Fl Lempty)):rest) tokens
reduce ((Db orop):(Dk LBRA):rest)                     tokens = reduce ((Di (Lone orop)):(Dk LBRA):rest) tokens
reduce decls@((Di _):(Dk LBRA):_)                     tokens = shift decls tokens
reduce decls@((Dk COMMA):(Di _):(Dk LBRA):_)          tokens = shift decls tokens
reduce ((Db x):(Dk COMMA):(Di list):(Dk LBRA):rest)   tokens = reduce ((Di (Lcons list x)):(Dk LBRA):rest) tokens
reduce ((Dk RBRA):(Di list):(Dk LBRA):rest)           tokens = reduce ((Df (Fl list)):rest) tokens

{- call -}
reduce decls@((Dn _):_)                  tokens@(LPAREN:_) = shift decls tokens
reduce decls@((Dk LPAREN):(Dn _):_)      tokens@(RPAREN:_) = shift decls tokens
reduce ((Dk LPAREN):(Dn neg):rest)                  tokens = reduce ((Dr Rempty):(Dk LPAREN):(Dn neg):rest) tokens
reduce decls@((Dr _):(Dk LPAREN):(Dn _):_)          tokens = shift decls tokens
reduce decls@((Db _):(Dr _):(Dk LPAREN):(Dn _):_)   tokens = shift decls tokens
reduce ((Dk COMMA):(Db orop):(Dr args):(Dk LPAREN):(Dn neg):rest)  tokens = reduce ((Dr (Rcons args orop)):(Dk LPAREN):(Dn neg):rest) tokens
reduce ((Dk RPAREN):(Dk LPAREN):(Dn neg):rest)                     tokens = reduce ((Dh (Call neg Rempty)):rest) tokens
reduce ((Dk RPAREN):(Db orop):(Dr args):(Dk LPAREN):(Dn neg):rest) tokens = reduce ((Dh (Call neg (Rcons args orop))):rest) tokens

{- terms -}
reduce decls@((Dt _):_)            tokens@(MULT:_) = shift decls tokens
reduce decls@((Dt _):_)            tokens@(DIV:_)  = shift decls tokens
reduce decls@((Dk MULT):(Dt _):_)           tokens = shift decls tokens
reduce decls@((Dk DIV):(Dt _):_)            tokens = shift decls tokens
reduce ((Df fact):(Dk MULT):(Dt term):rest) tokens = reduce ((Dt (Mult term fact)):rest) tokens
reduce ((Df fact):(Dk DIV):(Dt term):rest)  tokens = reduce ((Dt (Div term fact)):rest)  tokens

{- arithmetics -}
reduce decls@((Da _):_)            tokens@(PLUS:_)  = shift decls tokens
reduce decls@((Da _):_)            tokens@(MINUS:_) = shift decls tokens
reduce decls@((Dk PLUS):(Da _):_)            tokens = shift decls tokens
reduce decls@((Dk MINUS):(Da _):_)           tokens = shift decls tokens
reduce ((Dt term):(Dk PLUS):(Da arth):rest)  tokens = reduce ((Da (Add arth term)):rest)  tokens
reduce ((Dt term):(Dk MINUS):(Da arth):rest) tokens = reduce ((Da (Sub arth term)):rest)  tokens

{- comparisons -}
reduce decls@((Dc _):_)            tokens@(TLTE:_) = shift decls tokens
reduce decls@((Dc _):_)            tokens@(TGTE:_) = shift decls tokens
reduce decls@((Dc _):_)            tokens@(TNE:_)  = shift decls tokens
reduce decls@((Dc _):_)            tokens@(TLT:_)  = shift decls tokens
reduce decls@((Dc _):_)            tokens@(TEQ:_)  = shift decls tokens
reduce decls@((Dc _):_)            tokens@(TGT:_)  = shift decls tokens
reduce decls@((Dk TLTE):(Dc _):_)           tokens = shift decls tokens
reduce decls@((Dk TGTE):(Dc _):_)           tokens = shift decls tokens
reduce decls@((Dk TNE):(Dc _):_)            tokens = shift decls tokens
reduce decls@((Dk TLT):(Dc _):_)            tokens = shift decls tokens
reduce decls@((Dk TEQ):(Dc _):_)            tokens = shift decls tokens
reduce decls@((Dk TGT):(Dc _):_)            tokens = shift decls tokens
reduce ((Dj join):(Dk TLTE):(Dc comp):rest) tokens = reduce ((Dc (Lte comp join)):rest)  tokens
reduce ((Dj join):(Dk TGTE):(Dc comp):rest) tokens = reduce ((Dc (Gte comp join)):rest)  tokens
reduce ((Dj join):(Dk TNE):(Dc comp):rest)  tokens = reduce ((Dc (Neq  comp join)):rest)  tokens
reduce ((Dj join):(Dk TLT):(Dc comp):rest)  tokens = reduce ((Dc (Lt  comp join)):rest)  tokens
reduce ((Dj join):(Dk TEQ):(Dc comp):rest)  tokens = reduce ((Dc (Eq  comp join)):rest)  tokens
reduce ((Dj join):(Dk TGT):(Dc comp):rest)  tokens = reduce ((Dc (Gt  comp join)):rest)  tokens

{- not operation -}  -- not operation not allowed across lines
reduce decls@((Dk NOT):_)    tokens@(_:_) = shift decls tokens
reduce ((Dc comp):(Dk NOT):rest)   tokens = reduce ((Dz (Not comp)):rest) tokens

{- and operation -}
reduce decls@((Dd _):_)              tokens@(AND:_) = shift decls tokens
reduce decls@((Dk AND):(Dd _):_)             tokens = shift decls tokens
reduce ((Dz notop):(Dk AND):(Dd andop):rest) tokens = reduce ((Dd (And andop notop)):rest)  tokens

{- or operation -}
reduce decls@((Db _):_)             tokens@(OR:_) = shift decls tokens
reduce decls@((Dk OR):(Db _):_)            tokens = shift decls tokens
reduce ((Dd andop):(Dk OR):(Db orop):rest) tokens = reduce ((Db (Or orop andop)):rest)  tokens

{- parentheses -}
reduce decls@((Dk LPAREN):_)                    tokens = shift decls tokens
reduce decls@((Db _):(Dk LPAREN):_)             tokens = shift decls tokens
reduce ((Dk RPAREN):(Db orop):(Dk LPAREN):rest) tokens = reduce ((Dp (Pe orop)):rest) tokens

{- assign -}  -- assignments not allowed across lines
reduce decls@((Dn (Nd _)):_)          tokens@(COLON:_) = shift decls tokens
reduce decls@((De _):_)               tokens@(COLON:_) = shift decls tokens
reduce decls@((Dk COLON):(Dn (Nd _)):_)   tokens@(_:_) = shift decls tokens
reduce decls@((Dk COLON):(De _):_)        tokens@(_:_) = shift decls tokens
reduce ((Db orop):(Dk COLON):(Dn (Nd name)):rs) tokens = reduce ((Ds (Sa (AssnId name orop))):rs) tokens
reduce ((Db orop):(Dk COLON):(De ele):rs)       tokens = reduce ((Ds (Sa (AssnElem ele orop))):rs) tokens

{- join -}
reduce decls@((Dj _):_)            tokens@(CONCAT:_) = shift decls tokens
reduce decls@((Dk CONCAT):(Dj _):_)           tokens = shift decls tokens
reduce ((Da arth):(Dk CONCAT):(Dj join):rest) tokens = reduce ((Dj (Concat join arth)):rest) tokens

{- if -}
reduce decls@((Dk IF):_)                                                 tokens = shift decls tokens
reduce decls@((Db _):(Dk IF):_)                                          tokens = shift decls tokens
reduce decls@((Dk THEN):(Db _):(Dk IF):_)                                tokens = shift decls tokens
reduce decls@((Dl _):(Dk THEN):(Db _):(Dk IF):_)                         tokens = shift decls tokens
reduce ((Dk FI):(Dl line):(Dk THEN):(Db orop):(Dk IF):rest)              tokens = reduce ((Ds (Si (If orop line))):rest) tokens
reduce decls@((Dk OTHER):(Dl _):(Dk THEN):(Db _):(Dk IF):_)              tokens = shift decls tokens
reduce decls@((Dl _):(Dk OTHER):(Dl _):(Dk THEN):(Db _):(Dk IF):_)       tokens = shift decls tokens
reduce ((Dk FI):(Dl b):(Dk OTHER):(Dl a):(Dk THEN):(Db t):(Dk IF):rest)  tokens = reduce (Ds (Si (IfOther t a b)):rest) tokens
reduce decls@((Dk ELSE):(Dl _):(Dk THEN):(Db _):(Dk IF):_)               tokens = shift decls tokens
reduce ((Ds (Si ifStmt)):(Dk ELSE):(Dl a):(Dk THEN):(Db t):(Dk IF):rest) tokens = reduce (Ds (Si (IfElse t a ifStmt)):rest) tokens

reduce ((Dk OTHER):Dnewline:rest) tokens = reduce ((Dk OTHER):rest) tokens
reduce ((Dk ELSE):Dnewline:rest)  tokens = reduce ((Dk ELSE):rest) tokens
reduce ((Dk FI):Dnewline:rest)    tokens = reduce ((Dk FI):rest) tokens

{- while -}
reduce decls@((Dk WHILE):_)                         tokens = shift decls tokens
reduce decls@((Db _):(Dk WHILE):_)                  tokens = shift decls tokens
reduce decls@((Dk THEN):(Db _):(Dk WHILE):_)        tokens = shift decls tokens
reduce decls@((Dl _):(Dk THEN):(Db _):(Dk WHILE):_) tokens = shift decls tokens
reduce ((Dk REPEAT):(Dl line):(Dk THEN):(Db orop):(Dk WHILE):rest) tokens = reduce (Ds (Sw (While orop line)):rest) tokens

reduce ((Dk REPEAT):Dnewline:rest)           tokens = reduce ((Dk REPEAT):rest) tokens

{- to -}
reduce decls@((Dk TO):_)                                          tokens@(_:_) = shift decls tokens
reduce decls@((Dk (ID _)):(Dk TO):_)                                    tokens = shift decls tokens
reduce decls@((Dk WITH):(Dk (ID _)):(Dk TO):_)                          tokens = shift decls tokens
reduce ((Dk (ID param)):(Dk WITH):(Dk (ID name)):(Dk TO):rest)          tokens = reduce ((Dm (Pcons Pempty param)):(Dk WITH):(Dk (ID name)):(Dk TO):rest) tokens
reduce decls@((Dm _):(Dk WITH):(Dk (ID _)):(Dk TO):_)                   tokens = shift decls tokens
reduce decls@((Dk COMMA):(Dm _):(Dk WITH):(Dk (ID _)):(Dk TO):_)        tokens = shift decls tokens
reduce ((Dk (ID next)):(Dk COMMA):(Dm params):(Dk WITH):(Dk (ID name)):(Dk TO):rest)   tokens = reduce ((Dm (Pcons params next)):(Dk WITH):(Dk (ID name)):(Dk TO):rest) tokens

reduce decls@((Dk DO):(Dk (ID _)):(Dk TO):_)                            tokens = shift decls tokens
reduce decls@((Dl _):(Dk DO):(Dk (ID _)):(Dk TO):_)                     tokens = shift decls tokens
reduce ((Dk DONE):(Dl line):(Dk DO):(Dk (ID name)):(Dk TO):rest)        tokens = reduce ((Ds (St (Func name Pempty line))):rest) tokens

reduce decls@((Dk DO):(Dm _):(Dk WITH):(Dk (ID _)):(Dk TO):_)                          tokens = shift decls tokens
reduce decls@((Dl _):(Dk DO):(Dm _):(Dk WITH):(Dk (ID _)):(Dk TO):_)                   tokens = shift decls tokens
reduce ((Dk DONE):(Dl line):(Dk DO):(Dm params):(Dk WITH):(Dk (ID name)):(Dk TO):rest) tokens = reduce ((Ds (St (Func name params line))):rest) tokens

reduce ((Dk WITH):Dnewline:rest)    tokens = reduce ((Dk WITH):rest) tokens
reduce ((Dk DO):Dnewline:rest)      tokens = reduce ((Dk DO):rest) tokens
reduce ((Dk DONE):Dnewline:rest)    tokens = reduce ((Dk DONE):rest) tokens

{- return -}
reduce decls@((Dk RETURN):_)  tokens@(_:_) = shift decls tokens
reduce ((Db orop):(Dk RETURN):rest) tokens = reduce ((Ds (Sr (Return orop))):rest) tokens

{- comma line combination -}  -- comma cannot be the last character in a line
reduce decls@((Dl _):_)          tokens@(COMMA:_) = shift decls tokens
reduce decls@((Dk COMMA):(Dl _):_)   tokens@(_:_) = shift decls tokens
reduce ((Ds stmt):(Dk COMMA):(Dl line):rs) tokens = reduce ((Dl (Lm line stmt)):rs) tokens

{- automatic line combination -}
reduce decls@(Dnewline:(Dl _):_)   tokens = shift decls tokens
reduce ((Ds stmt):Dnewline:(Dl line):rest) tokens = reduce ((Dl (Lm line stmt)):rest) tokens

{- negation -}  -- negation not allowed across lines
reduce decls@((Dk MINUS):_)                  tokens@(_:_) = shift decls tokens
reduce ((Dn neg):(Dk MINUS):rest) tokens | negatable rest = reduce ((Df (Fn neg)):rest) tokens
    where negatable ((Da _):_) = False
          negatable _          = True

{- promotions -}
reduce ((Dk (ID tid)):rest)  tokens = reduce ((Dn (Nd tid)):rest)   tokens
reduce ((Dk (INT int)):rest) tokens = reduce ((Dn (Ni int)):rest)   tokens
reduce ((Dp paren):rest)     tokens = reduce ((Dn (Np paren)):rest) tokens
reduce ((De ele):rest)       tokens = reduce ((Dn (Ne ele)):rest) tokens
reduce ((Do obj):rest)       tokens = reduce ((Dn (No obj)):rest)   tokens
reduce ((Dh call):rest)      tokens = reduce ((Dn (Nc call)):rest)  tokens
reduce ((Dn neg):rest)       tokens = reduce ((Df (Fp neg)):rest)   tokens
reduce ((Dk NOTHING):rest)   tokens = reduce ((Df (Fnothing)):rest) tokens
reduce ((Dk (STR s)):rest)   tokens = reduce ((Df (Fs s)):rest)     tokens
reduce ((Dk TRUE):rest)      tokens = reduce ((Df (Fb True)):rest)  tokens
reduce ((Dk FALSE):rest)     tokens = reduce ((Df (Fb False)):rest) tokens
reduce ((Df fact):rest)      tokens = reduce ((Dt (Tf fact)):rest)  tokens
reduce ((Dt term):rest)      tokens = reduce ((Da (At term)):rest)  tokens
reduce ((Da arth):rest)      tokens = reduce ((Dj (Ja arth)):rest)  tokens
reduce ((Dj join):rest)      tokens = reduce ((Dc (Cj join)):rest)  tokens
reduce ((Dc comp):rest)      tokens = reduce ((Dz (Mc comp)):rest)  tokens
reduce ((Dz notop):rest)     tokens = reduce ((Dd (An notop)):rest)  tokens
reduce ((Dd andop):rest)     tokens = reduce ((Db (Oa andop)):rest) tokens
reduce ((Db orop):rest)      tokens = reduce ((Ds (Se orop)):rest)  tokens
reduce ((Ds stmt):rest)      tokens = reduce ((Dl (Ls stmt)):rest)  tokens

{- base cases -}
reduce decls@[Dl _] []      = return decls
reduce decls        tokens  = parseFail $ (show decls) ++ " " ++ (show tokens)

{- demotions -}
demote :: Decl -> Decl
demote (Ds (Se orop))  = demote (Db orop)
demote (Db (Oa andop)) = demote (Dd andop)
demote (Dd (An notop)) = demote (Dz notop)
demote (Dz (Mc comp))  = demote (Dc comp)
demote (Dc (Cj join))  = demote (Dj join)
demote (Dj (Ja arth))  = demote (Da arth)
demote (Da (At term))  = demote (Dt term)
demote (Dt (Tf fact))  = demote (Df fact)
demote x               = x
