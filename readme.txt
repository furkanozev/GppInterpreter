
*********** PART 1 *******************
For compile and run:

	lex gpp_lexer.l 		=> It will reproduce a lex.yy.c file.
	yacc -d gpp_parser.y
	gcc lex.yy.c y.tab.c -w
	./a.out

	OR

	./a.out

If you want to terminate the program, you must enter: (exit)

I did this part using regular expressions and syntax rules in accordance with homework pdf and g++ syntax pdf.


*********** PART 2 *******************
For run:

	clisp gpp_parser.lisp

After run program, You must choose between 2 selections.
1. (gppinterpreter)
2. (gppinterpreter "filename.g++")

! Please attention to the spaces when entering these (above) selections. For example ( gppinterpreter ) will give you error message.

In the 1st selection, the program works as an interpreter.
In the 2st selection, File will be loaded by the interpreter and interpreted right away.

If you want to terminate the program, you must enter: ( exit )


I added execution images to related files.
I created the test sample in the homework pdf as a file named "test.g++".
I uploaded this file to the part folders.