type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val nestLevel = ref 0
val inString = ref false
val str = ref ""
val stringPos = ref 0
fun newlCounter s = List.length(String.tokens (fn c => if c = #"\n" then true else false) s) -1
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let 
				val pos = hd(!linePos) 
			in 
				if (!nestLevel <> 0)
				then ErrorMsg.error pos ("UNCLOSED COMMENT")
				else if (!inString)
				then ErrorMsg.error pos ("UNCLOSED STRING")
				else ();
				Tokens.EOF(pos,pos) 
			end

fun convertAscii s= 
	let
	val pos = hd(!linePos)
	val ascii = valOf(Int.fromString(String.substring(s,1,size s -1)))
	val () = if ascii > 255
	then ErrorMsg.error pos "illegal ascii escape"
	else ();
	in
	if ascii < 255
	then Char.toString(Char.chr(ascii))
	else ""
	end

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
alpha = [A-Za-z];
digit = [0-9];
quote = \" ;
ws = [\ \t];
newln = \n ;

%s COMMENT STRING;
%%

<INITIAL, COMMENT> {newln}	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> {ws}+ => (continue());
<INITIAL> {quote} => (stringPos := yypos;inString := true; str := ""; YYBEGIN STRING; continue());
<STRING> {quote} => (inString := false; YYBEGIN INITIAL; Tokens.STRING((!str), !stringPos, yypos));

<STRING> {newln} => (lineNum := !lineNum+1; linePos := yypos :: !linePos; ErrorMsg.error yypos ("Cannot have newline in string literal"); continue());
<STRING> \\n => (str := !str ^ "\n"; continue());
<STRING> \\t => (str := !str ^ "\t"; continue());
<STRING> "\\\"" => (str := !str ^ "\""; continue());
<STRING> \\\\ => (str := !str ^ "\\"; continue());
<STRING> \\{digit}{3} => ( str := !str ^ (convertAscii yytext); continue());
<STRING> \\[\n|\t|\ |\f]+\\ => (lineNum := !lineNum + newlCounter yytext;continue());
<STRING> \\[\n|\t|\ |\f]+[^\\] => (ErrorMsg.error yypos "unclosed form feed between string";continue());
<STRING> . => (str := !str ^ yytext; continue());

<INITIAL, COMMENT> "/*" => (nestLevel := !nestLevel + 1; YYBEGIN COMMENT; continue());
<COMMENT> "*/" => (nestLevel := !nestLevel - 1; if !nestLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> . => (continue());

<INITIAL> 	","	=> 	(Tokens.COMMA(yypos,yypos+1));
<INITIAL>	":"	=> 	(Tokens.COLON(yypos,yypos+1));
<INITIAL> 	";" => 	(Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>   "("	=> 	(Tokens.LPAREN(yypos,yypos+1));
<INITIAL>   ")" => 	(Tokens.RPAREN(yypos,yypos+1));
<INITIAL>   "[" => 	(Tokens.LBRACK(yypos,yypos+1));
<INITIAL>   "]" => 	(Tokens.RBRACK(yypos,yypos+1));
<INITIAL>   "{"	=> 	(Tokens.LBRACE(yypos,yypos+1));
<INITIAL>   "}" => 	(Tokens.RBRACE(yypos,yypos+1));
<INITIAL>   "." => 	(Tokens.DOT(yypos,yypos+1));
<INITIAL>   "+" => 	(Tokens.PLUS(yypos,yypos+1));
<INITIAL>   "-" => 	(Tokens.MINUS(yypos,yypos+1));
<INITIAL>   "*" => 	(Tokens.TIMES(yypos,yypos+1));
<INITIAL>   "/" => 	(Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>   "=" => 	(Tokens.EQ(yypos,yypos+1));
<INITIAL>   "<>" => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>   "<=" => (Tokens.LE(yypos,yypos+2));
<INITIAL>   "<" => 	(Tokens.LT(yypos,yypos+1));
<INITIAL>   ">=" => (Tokens.GE(yypos,yypos+2));
<INITIAL>   ">" => 	(Tokens.GT(yypos,yypos+1));
<INITIAL>   "&" => 	(Tokens.AND(yypos,yypos+1));
<INITIAL>    "|" => 	(Tokens.OR(yypos,yypos+1));
<INITIAL>   ":=" => (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL>	type => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>	var => (Tokens.VAR(yypos, yypos+3));
<INITIAL>	function => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>	break => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>	of => (Tokens.OF(yypos, yypos+2));
<INITIAL>	end => (Tokens.END(yypos, yypos+3));
<INITIAL>	in => (Tokens.IN(yypos, yypos+2));
<INITIAL>	nil => (Tokens.NIL(yypos, yypos+3));
<INITIAL>	let => (Tokens.LET(yypos, yypos+3));
<INITIAL>	do => (Tokens.DO(yypos, yypos+2));
<INITIAL>	to => (Tokens.TO(yypos, yypos+2));
<INITIAL>	for => (Tokens.FOR(yypos, yypos+3));
<INITIAL>	while => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>	else => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>	then => (Tokens.THEN(yypos, yypos+4));
<INITIAL>	if => (Tokens.IF(yypos, yypos+2));
<INITIAL> 	array => (Tokens.ARRAY(yypos, yypos+5));

<INITIAL>    {alpha}({alpha}|{digit}|"_")* => (Tokens.ID(yytext,yypos, yypos + size yytext));
<INITIAL>    {digit}+ => (Tokens.INT(Option.getOpt(Int.fromString(yytext), 0), yypos, yypos + size yytext));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
