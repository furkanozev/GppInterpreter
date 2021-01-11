%{
	#include <stdio.h>
	#include <string.h>

	int string_in(char *my_str, char string_list[1000][200], int num_strings)
	{
		int i;
	    for(i = 0; i < num_strings; i++ )
	        if (strcmp(my_str, string_list[i]) == 0 )
	            return i;

	    return -1;
	}

	void upper_string(char s[])
	{
   		int c = 0;
   
   		while (s[c] != '\0') {
      		if (s[c] >= 'a' && s[c] <= 'z') {
         		s[c] = s[c] - 32;
      		}
      		c++;
   		}
	}

	void copy(int a[], int b[], int size)
	{
		int i;
		for(i = 0; i < size && i < 999; i++)
			a[i] = b[i];
	}

	struct a1
	{ 
	   char iden[1000][200]; 
	   int values[1000]; 
	   int count;
	};

	struct a1 idens = {"", 0, 0};

	int i = 2;
	int y = 0;
	int z = 0;
	int sum = 0;

%}


%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND
%token KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_DBLMULT
%token OP_MULT OP_OP OP_CP OP_OC OP_CC OP_COMMA COMMENT IDENTIFIER VALUE
%token CUT FILENAME
%start START

%union 
{
	struct {
		int flag;
        int number;
        char *string;
        int values[1000];
    };
}

%%

START: | START INPUT {printf("[%d]> ", i); i += 1; y = 0; z = 0; sum = 0;};

INPUT:
		EXPI{
				if($<flag>$ == 0)
				{printf("%d\n", $<number>1); $<number>$ = $<number>1; $<flag>$ = 0;}
				else if($<flag>$ == 1)
				{printf("%s\n", $<string>1); $<string>$ = $<string>1; $<flag>$ = 1;}
				else if($<flag>$ == 2)
				{
					if($<number>$ == 1) printf("TRUE\n");
					else printf("FALSE\n");
					$<number>$ = $<number>1;
					$<flag>$ = 2;
				}
				else if($<flag>$ == 3)
				{
					int j;
					printf("(");
					for(j = 0; j < z; j += 1)
    					{printf("%d", $<values>1[j]);
    					if(j != z-1) printf(" ");}
    				printf(")\n");
    				copy($<values>$, $<values>1, z);
					$<flag>$ = 3;
				}
			}
		| EXPLISTI {
						if($<flag>$ == 0)
						{printf("%d\n", $<number>1); $<number>$ = $<number>1; $<flag>$ = 0;}
						else if($<flag>$ == 1)
						{printf("%s\n", $<string>1); $<string>$ = $<string>1; $<flag>$ = 1;}
						else if($<flag>$ == 2)
						{
							if($<number>$ == 1) printf("T\n");
							else printf("NIL\n");
							$<number>$ = $<number>1;
							$<flag>$ = 2;
						}
						else if($<flag>$ == 3)
						{
							int j;
							printf("(");
							for(j = 0; j < z; j += 1)
            					{printf("%d", $<values>1[j]);
            					if(j != z-1) printf(" ");}
            				printf(")\n");
            				copy($<values>$, $<values>1, z);
							$<flag>$ = 3;
						}
					}
		| COMMENT {$<flag>$ = 5;};

LISTVALUE:
		OP_OP KW_LIST VALUES OP_CP {
										copy($<values>$, $<values>3, y);
										$<flag>$ = 3;
									}
		| CUT OP_OP VALUES OP_CP {
										copy($<values>$, $<values>3, y);
										$<flag>$ = 3;
								 	};

VALUES:
		VALUES VALUE{
						$<values>$[y] = $<number>2;
						y += 1;
						$<flag>$ = 3;
					 }
		| VALUES IDENTIFIER{
							upper_string($<string>2);
							int res = string_in($<string>2, idens.iden, idens.count);
							if(res != -1)
							{
								$<number>2 = idens.values[res];
								$<flag>$ = 0;
							}
							else
							{
								printf(" variable %s has no value\n", $<string>2);
								exit(0);
							}
							$<values>$[y] = $<number>2;
							y += 1;
							$<flag>$ = 3;
						 }
		| VALUE {$<values>$[y] = $<number>1; y += 1; $<flag>$ = 3;};

IDENTIFIERS:
		IDENTIFIERS IDENTIFIER{
								upper_string($<string>1);
								$<string>$ = $<string>1;
								strcat($<string>$, " ");
								strcat($<string>$, $<string>2);
								$<flag>$ = 1;
							  }
		| IDENTIFIER {
						upper_string($<string>1);
						int res = string_in($<string>1, idens.iden, idens.count);
						if(res == -1)
						{
							strcpy(idens.iden[idens.count], $<string>1);
							idens.values[idens.count] = -1;
							idens.count += 1;
						}
						$<string>$ = $<string>1; $<flag>$ = 1;
					 };

EXPI:
		OP_OP OP_PLUS EXPI EXPI OP_CP {$<number>$ = $<number>3 + $<number>4; $<flag>$ = 0;}
		| OP_OP OP_MINUS EXPI EXPI OP_CP {$<number>$ = $<number>3 - $<number>4; $<flag>$ = 0;}
		| OP_OP OP_MULT EXPI EXPI OP_CP {$<number>$ = $<number>3 * $<number>4; $<flag>$ = 0;}
		| OP_OP OP_DIV EXPI EXPI OP_CP {
										if($<number>4 == 0)
											{printf("Can not divide by zero\n");
											exit(0);}
										else
											{$<number>$ = $<number>3 / $<number>4;
											$<flag>$ = 0;}
										}
		| OP_OP OP_DBLMULT EXPI EXPI OP_CP {
											int x = $<number>3;
											int j;
											for(j = 1;j<$<number>4;j++)
												x *=$<number>3;
											$<number>$ = x;
											$<flag>$ = 0;
										   }
		| OP_OP IDENTIFIER EXPLISTI OP_CP {upper_string($<string>2); $<string>$ = $<string>2; $<flag>$ = 1;}
		| OP_OP IDENTIFIER EXPI OP_CP {upper_string($<string>2); $<string>$ = $<string>2; $<flag>$ = 1;}
		| OP_OP KW_SET IDENTIFIER EXPI OP_CP{
											upper_string($<string>3);
											int res = string_in($<string>3, idens.iden, idens.count);
											if(res == -1)
											{
												strcpy(idens.iden[idens.count], $<string>3);
												idens.values[idens.count] = $<number>4;
												idens.count += 1;
											}
											else
												{idens.values[res] = $<number>4;}

											$<number>$ = $<number>4;
											$<flag>$ = 0;
											}
		| OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP{
													copy($<values>$, $<values>4, z);
													$<flag>$ = 3;
												}
		| OP_OP KW_IF EXPB EXPLISTI OP_CP{
											if($<number>3 == 1)
											{
												copy($<values>$, $<values>4, z);
												$<flag>$ = 3;
											}
											else
											{
												$<number>$ = 0;
												$<flag>$ = 3;
											}
										 }
		| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
													if($<number>3 == 1)
													{
														copy($<values>$, $<values>4, z);
														$<flag>$ = 3;
													}
													else
													{
														copy($<values>$, $<values>5, z);
														$<flag>$ = 3;
													}
												 }
		| OP_OP KW_IF EXPB EXPI OP_CP{
										if($<number>3 == 1)
										{
											$<number>$ = $<number>4;
											$<flag>$ = 0;
										}
										else
										{
											$<number>$ = 0;
											$<flag>$ = 3;
										}
									 }
		| OP_OP KW_IF EXPB EXPI EXPI OP_CP{
											if($<number>3 == 1)
											{
												$<number>$ = $<number>4;;
												$<flag>$ = 0;
											}
											else
											{
												$<number>$ = $<number>5;
												$<flag>$ = 0;
											}
										 }
		| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP {
																copy($<values>$, $<values>8, z);
																$<flag>$ = 3;
															 }
		| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{
												upper_string($<string>3);
												strcpy(idens.iden[idens.count], $<string>3);
												idens.values[idens.count] = $<number>4;
												idens.count += 1;
												$<string>$ = $<string>3;
												$<flag>$ = 1;
										  		}
		| OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{
												upper_string($<string>3);
												$<string>$ = $<string>3;
												$<flag>$ = 1;
										  		}
		| OP_OP KW_LOAD OP_OC FILENAME OP_CC OP_CP {
														FILE* fp = fopen($<string>4, "r");
														$<number>$ = 0;
														$<flag>$ = 2;
														if(fp == NULL) printf("A file with name %s does not exist\n", $<string>4);
											    		else{
											    			printf(";; Loaded file %s\n", $<string>4);
											    			$<number>$ = 1;
											    		}
												   }
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPI OP_CP {
																		upper_string($<string>3);
																		strcpy(idens.iden[idens.count], $<string>3);
																		idens.values[idens.count] = -1;
																		idens.count += 1;
																		$<string>$ = $<string>3;
																		$<flag>$ = 1;
																		}
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPLISTI OP_CP {
																			upper_string($<string>3);
																			strcpy(idens.iden[idens.count], $<string>3);
																			idens.values[idens.count] = -1;
																			idens.count += 1;
																			$<string>$ = $<string>3;
																			$<flag>$ = 1;
																			}
		| OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP {
														upper_string($<string>3);
														strcpy(idens.iden[idens.count], $<string>3);
														idens.values[idens.count] = -1;
														idens.count += 1;
														$<string>$ = $<string>3;
														$<flag>$ = 1;
													}
		| OP_OP KW_EXIT OP_CP {printf("Bye.\n"); exit(1);}
		| OP_OP KW_DISP EXPI OP_CP {
										if($<flag>3 == 0)
										{$<number>$ = $<number>3; $<flag>$ = 0;}
										else if($<flag>3 == 1)
										{$<string>$ = $<string>3; $<flag>$ = 1;}
										else if($<flag>3 == 2)
										{$<number>$ = $<number>3; $<flag>$ = 2;}
										else if($<flag>3 == 3)
										{copy($<values>$, $<values>3, z); $<flag>$ = 3;}
									}
		| OP_OP KW_DISP EXPLISTI OP_CP {
										copy($<values>$, $<values>3, z); $<flag>$ = 3;
									   }
		| EXPB {$<number>$ = $<number>1; $<flag>$ = $<flag>1;};

EXPB:
		OP_OP KW_AND EXPB EXPB OP_CP {$<number>$ = $<number>3 && $<number>4; $<flag>$ = 2;}
		| OP_OP KW_OR EXPB EXPB OP_CP {$<number>$ = $<number>3 || $<number>4; $<flag>$ = 2;}
		| OP_OP KW_NOT EXPB OP_CP {if($<number>3 == 0) $<number>$ = 1; else $<number>$ = 0; $<flag>$ = 2;}
		| OP_OP KW_EQUAL EXPB EXPB OP_CP {if($<number>2 == $<number>3) $<number>$ = 0; else $<number>$ = 1; $<flag>$ = 2;}
		| OP_OP KW_LESS EXPB EXPB OP_CP {if($<number>3 < $<number>4) $<number>$ = 1; else $<number>$ = 0; $<flag>$ = 2;}
		| VALUE {$<number>$ = $<number>1; $<flag>$ = 0;}	
		| KW_TRUE {$<number>$ = 1; $<flag>$ = 2;}
		| KW_FALSE {$<number>$ = 0; $<flag>$ = 2;}
		| KW_NIL {$<number>$ = 0; $<flag>$ = 2;}
		| IDENTIFIER {
						upper_string($<string>1);
						int res = string_in($<string>1, idens.iden, idens.count);
						if(res != -1)
						{
							$<number>$ = idens.values[res];
							$<flag>$ = 0;
						}
						else
						{
							printf(" variable %s has no value\n", $<string>1);
							exit(0);
						}
					 };

EXPLISTI:
		OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++)
													{$<values>3[j] = $<values>4[x];}
													copy($<values>$, $<values>3, sum);
													$<flag>$ = 3;
													z = sum;
												 }
		| OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++)
													{$<values>3[j] = $<values>4[x];}
													copy($<values>$, $<values>3, sum);
													$<flag>$ = 3;
													z = sum;
												  }
		| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {
													int j;
													for(j = 0; j < z; j++)
													{$<values>$[j+1] = $<values>4[j];}
													$<values>$[0] = $<number>3;
													$<flag>$ = 3;
													z += 1;
											  }
		| LISTVALUE {
						copy($<values>$, $<values>1, y);
						$<flag>$ = 3;
						z = y;
						sum += y;
						y = 0;
					};

%%

int yyerror(char *msg) 
{ 
  printf("SYNTAX_ERROR Expression not recognized\n");
  exit(0); 
}

int yywrap(){
	return 1;
} 

int main(void) 
 {
 	printf("[1]> ");
  	yyparse(); 
 } 