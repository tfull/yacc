%header
module Parser(parse) where

import Type
import qualified Token

%token_class Token.Token
%token_value Token.tokenV
%token_position Token.lineS Token.charS Token.lineG Token.charG

%tree_class Tree
%tree_value treeV
%tree_position lineS charS lineG charG

%terminal_map
plus Token.Plus
minus Token.Minus
star Token.Star
slash Token.Slash
percent Token.Percent
int Token.Int Int
lpar Token.LPar
rpar Token.RPar
$ Token.End

%rule
S' % S $   %   $1
S  % S plus A % Add $1 $3
S  % S minus A % Sub $1 $3
S  % A % $1
A  % A star B % Mul $1 $3
A  % A slash B % Div $1 $3
A  % A percent B % Mod $1 $3
A  % B % $1
B  % minus C % Minus $2
B  % C % $1
C  % int % Int $1
C  % lpar S rpar % $2
