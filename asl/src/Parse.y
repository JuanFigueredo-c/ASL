{
module Parse where
import Common
import AST
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parseStmt Term
%name parseComm Comms
%name parseScene Scene
%name parseFile File

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    play        {TPlay}
    ';'         {TSeq}
    '||'        {TPar}
    loop        {TLoop}
    move        {TMove}
    rotate      {TRotate}
    scale       {TScale}
    static      {TStatic}
    orbit       {TOrbit}
    circle      {TCircle}
    rect        {TRect}
    triang      {TTriang}
    poly        {TPoly}
    line        {TLine}
    stack       {TStack}
    offset      {TOffset}
    bind        {TBind}
    rot         {TRot}
    resize      {TResize}
    paint       {TPaint}
    place       {TPlace}
    full        {TFill}
    outline     {TOutline}
    '('         {TOpen}
    ')'         {TClose}
    '{'         {TPointO}
    '}'         {TPointC}
    '['         {TAnsO}
    ']'         {TAnsC}
    '='         {TAssgn}
    ':'         {TType}
    '<<'        {TUpdate}
    ','         {TComma}
    '#'         {THash}
    float       {TFloat $$}
    int         {TInt $$}
    'Image'     {TImage}
    'Anim'      {TAnim}
    'Action'    {TAction}
    'Color'     {TColor}
    scene       {TScene}
    name        {TVarName $$}


%left '='
%left '||' 
%left ';' 
%right '<<'
%left loop
%left offset
%left bind 
%left stack 
%left rot paint resize
%%


File     :: { File }
         : Scene Terms Play                       {File $1 $2 $3}

Scene     :: { Scene }
          : scene int int Exp                     {Sc $2 $3 $4}
          | error {error "Parse Error: File must begin with a Scene declaration."}

Terms     :: { [Terms] }
          : Term Terms                             {$1 : $2}
          |                                        {[]}

Term        :: { Terms }
            : Def                                  {Right $1}
            | Comms                                {Left $1}

Def         :: {Decl}
            : name ':' Type '=' Exp                {Decl $1 $3 $5}

Atom        :: { Exp }
            : name                                 {Var $1}
            | '(' Exp ')'                          {$2}
            | ASLColor                             {$1}
            | ImageExp                             {$1}
            | AnimExp                              {$1} 
            | ActionExp                            {$1}

Exp         :: { Exp }
            : Exp ';' Exp                          {Seq $1 $3}
            | Exp '||' Exp                         {Par $1 $3}
            | loop Exp int                         {Loop $2 $3}
            | stack Exp Exp                        {Stack $2 $3}   
            | offset Exp Exp Point                 {Offset $2 $3 $4}
            | bind Exp Point Exp Point             {Bind $2 $3 $4 $5}
            | rot Exp float                        {Rot $2 $3}
            | resize Exp float                     {RSize $2 $3}
            | paint Exp Exp                        {Paint $2 $3}
            | Atom                                 {$1}

Type        :: { Type }
            : 'Image'                              {ImageT}
            | 'Action'                             {ActionT}
            | 'Anim'                               {AnimT}
            | 'Color'                              {ColorT}
            | error {error "Parse Error: Invalid Type"}

ImageExp      :: { Exp }
            : circle float float Fill Atom              {Circle $2 $3 $4 $5}
            | rect float float float Fill Atom          {Rect $2 $3 $4 $5 $6}
            | line float float float Atom               {Line $2 $3 $4 $5}
            | triang float float float Atom             {Triang $2 $3 $4 $5}
            | poly int float Atom                       {Polygon $2 $3 $4}

ActionExp   :: { Exp }
            : rotate float float                   {Rotate $2 $3}
            | move Point float                     {Move $2 $3 }
            | scale float float float              {Scale $2 $3 $4}  
            | orbit Point float float              {Orbit $2 $3 $4}
            | static float                         {Static $2 }                                

AnimExp     :: { Exp } 
            : place Exp Point float float          {Place $2 $3 $4 $5}

ASLColor    :: { Exp }
            : '#' int ',' int ',' int             {Color $2 $4 $6}

Point       :: { Point }
            : '{' float ',' float '}'              {Point $2 $4}

Comms       :: { Comm }
            : name '<<' Exp                {Update $1 $3}

Anims_      :: { [ Name ]}                  
            : ',' name Anims_               {$2 : $3} 
            |                               {[]}

Anims       :: { [ Name ] }
            :  name Anims_               {$1 : $2}
            | error {error "Parse Error: 'play' has no animations."}
 

Play        :: {Comm}
            : play '[' Anims ']'                    {Play $3}
            | error {error "Parse Error: File must end with a 'play' expression."}

Fill        :: { Fill }
            : full                                {Full}
            | outline                             {Outline}

                         
{

data Token = TPlay
            |   TSeq
            |   TPar
            |   TLoop
            |   TMove
            |   TRotate
            |   TScale
            |   TStatic
            |   TOrbit
            |   TUpdate
            |   TCircle
            |   TRect
            |   TLine
            |   TTriang
            |   TPoly
            |   TStack
            |   TOffset
            |   TBind
            |   TRot
            |   TResize
            |   TPaint
            |   TFill
            |   TOutline
            |   TPlace
            |   TThen
            |   TAPar
            |   TOpen
            |   TClose
            |   TComma
            |   THash
            |   TDot
            |   TAssgn
            |   TType
            |   TImage
            |   TAnim
            |   TAction
            |   TColor
            |   TFloat Float
            |   TInt Int
            |   TVarName String
            |   TEOF
            |   TScene
            |   TPointC
            |   TPointO
            |   TAnsO
            |   TAnsC
            deriving Show

data ParseResult a = Ok a | Failed String deriving Show
type P a = String -> LineNumber -> ParseResult a

type LineNumber = Int

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
   case m s l of
       Ok a     -> k a s l
       Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
   case m s l of
      Ok a     -> Ok a
      Failed e -> k e s l

happyError :: P a
happyError = \ s int -> Failed $ "Line "++(show (int::LineNumber))++": Parse error: " ++ (take 15 s)

lexer :: (Token -> P a ) -> P a
lexer cont s = case s of
                ('-':('-':cs))   -> lexer cont $ dropWhile ((/=) '\n') cs
                []               -> cont TEOF []
                ('\n':cs)        -> (\line -> lexer cont cs (line + 1))
                (c:cs) 
                        | isSeparator c -> lexer cont cs
                        | isAlpha     c -> lexVar (c:cs)
                        | isDigit c || c == '-'  -> lexNum (c:cs)
                ('(':cs)         ->  cont TOpen cs
                (')':cs)         ->  cont TClose cs
                ('{':cs)         ->  cont TPointO cs
                ('}':cs)         ->  cont TPointC cs
                ('[':cs)         ->  cont TAnsO cs
                (']':cs)         ->  cont TAnsC cs
                ('=':cs)         ->  cont TAssgn cs
                (';':cs)         ->  cont TSeq cs
                (':':cs)         ->  cont TType cs
                (',':cs)         ->  cont TComma cs
                ('|':('|':cs))   ->  cont TPar cs
                ('<':('<':cs))   ->  cont TUpdate cs
                ('#':cs)         ->  cont THash cs
                ('.':cs)         ->  cont TDot cs
                unknown          -> \line -> Failed $ 
                 "Line "++(show line)++": Cannot recognize:  "++ (show $ take 10 unknown) ++ "..."
                where lexVar cs = case span isVariable cs of
                                ("play",rest)       -> cont TPlay rest
                                ("loop",rest)       -> cont TLoop rest
                                ("move",rest)       -> cont TMove rest
                                ("rotate",rest)     -> cont TRotate rest
                                ("scale",rest)      -> cont TScale rest
                                ("static",rest)     -> cont TStatic rest
                                ("orbit",rest)      -> cont TOrbit rest
                                ("circle",rest)     -> cont TCircle rest
                                ("rect",rest)       -> cont TRect rest
                                ("line",rest)       -> cont TLine rest
                                ("triang",rest)     -> cont TTriang rest
                                ("poly",rest)       -> cont TPoly rest
                                ("stack",rest)      -> cont TStack rest
                                ("offset",rest)     -> cont TOffset rest
                                ("bind",rest)       -> cont TBind rest
                                ("rot", rest)       -> cont TRot rest
                                ("resize", rest)    -> cont TResize rest
                                ("paint", rest)     -> cont TPaint rest
                                ("place",rest)      -> cont TPlace rest
                                ("full",rest)       -> cont TFill rest
                                ("outline",rest)    -> cont TOutline rest
                                ("Image",rest)      -> cont TImage rest
                                ("Action",rest)     -> cont TAction rest
                                ("Anim",rest)       -> cont TAnim rest
                                ("Color",rest)      -> cont TColor rest
                                ("scene",rest)      -> cont TScene rest
                                (var, rest)         -> cont (TVarName var) rest
                                _                   -> (\line -> Failed $ "Line " ++ (show line) ++ ": Invalid variable syntax: " ++ (take 5 cs))
                      lexNum ('-':cs) = case span isDigit cs of
                                (digits, '.':rest) -> case span isDigit rest of
                                        ("",   more)     -> (\line -> Failed $ "Line " ++ (show line) ++ ": Invalid float syntax: " ++ digits ++ "." ++ (take 3 rest) ++ "...")
                                        (frac, more)     -> cont (TFloat $ read ("-" ++ digits ++ "." ++ frac)) more
                                (digits, rest)     -> cont (TInt $ read ("-" ++ digits)) rest
                                _                  -> (\line -> Failed $ "Line " ++ (show line) ++ ": Invalid int syntax: " ++ (take 5 cs) ++ "...")
                      lexNum cs = case span isDigit cs of
                                (digits, '.':rest) -> case span isDigit rest of
                                        ("",   more)     -> (\line -> Failed $ "Line " ++ (show line) ++ ": Invalid float syntax: " ++ digits ++ "." ++ (take 3 rest) ++ "...")
                                        (frac, more)     -> cont (TFloat $ read (digits ++ "." ++ frac)) more
                                (digits, rest)     -> cont (TInt $ read digits) rest
                                _                  -> (\line -> Failed $ "Line " ++ (show line) ++ ": Invalid int syntax: " ++ (take 5 cs) ++ "...")
                      isVariable :: Char -> Bool
                      isVariable c = isAlpha c || isDigit c || c == '-' || c == '_'


stmt_parse s = parseStmt s 1
comm_parse s = parseComm s 1
scene_parse s = parseScene s 1
file_parse s = parseFile s 1
}
