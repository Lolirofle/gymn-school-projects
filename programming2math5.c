// Recursive functions (2013-09-02)

#include <stdio.h>
#include <string.h>

//Boolean type (Not included earlier than C98)
#define bool unsigned char
#define false 0
#define true 1

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 0) The non-complete factorial function
//
	unsigned long long int factorial(unsigned short n){
		return n>1?n*factorial(n-1):n;
	}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 1) Write a function that takes a string and the length of the string and returns the string reversed
//
//    These functions won't work if str==out
	//With return value
	char* string_reverse1(const char* str,unsigned long len,char* out){
		if(len>0){//If there's still characters left
			out[len-1]=*str;
			return string_reverse1(str+1,len-1,out);//Call function recursively with pointer to string incremented and therefore also with a shorter length
		}else
			return out;
	}

	//Without return value
	void string_reverse2(const char* str,unsigned long len,char* out){
		if(len>0){
			out[len-1]=*str;
			string_reverse2(str+1,len-1,out);
		}
	}

	//Null terminated strings (Not tail recursive) (Returns pointer to end of string when completed) (Faster than string_reverse2 when not optimized with gcc -O3 but slower than the others with -O3)
	char* string_reverse3(const char* str,char* out){
		if(*str=='\0')//If null, end of string and return
			return out;

		char* o=string_reverse3(str+1,out);//This will call the function going all the way to end of string incrementing the pointer to string while getting the correct position for the pointer of out from the next layer of recursivity
		*o=*str;//Copy character
		return o+1;//Returns next out position for the previous layer of recursivity
	}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 2) Write a function that takes a string and counts the number of white spaces that appear
//	  in the string. Return that number
//
	//With while loop
	unsigned int string_count_char1(const char* str,char c){
		while(1){
			if(*str==c)//If current char is c
				return 1+string_count_char1(str+1,c);//Non tail-recursive call when incrementing by 1

			if(*str=='\0')//If end of string, return 0 and do not call recursively again
				return 0;

			str++;
		}
	}

	//Full recursive (One function call for one character)
	unsigned int string_count_char2(const char* str,char c){
		if(*str==c)
			return 1+string_count_char2(str+1,c);
		
		if(*str=='\0')
			return 0;

		return string_count_char2(str+1,c);
	}

	//My preferred method (tail recursive)
	#define string_count_char3(str,c) _string_count_char3(str,c,0)
	unsigned int _string_count_char3(const char* str,char c,unsigned int count){
		//If end of string, return the count and go all the way back to the first call
		//else call the function recursively, incrementing pointer with string one position
		//and increase counter if current character is the specified character c
		return *str=='\0'? count : _string_count_char3(str+1,c,*str==c? count+1 : count);
	}

	//...and then the real solution (Inlining if possible because this is just a call to another function)
	unsigned int string_count_whitespace(const char* str){
		return _string_count_char3(str,' ',0);
	}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// 3) Write a function that takes a string and its length and finds out if it is a palindrome
//
// Palindrome examples: anna, 123321, (12321, radar, level, racecar, madam, refer)
	bool string_isPalindrome(const char* str,unsigned int len){
		//DEBUGGING: printf("%s %i (%i,%i)\n",str,len,0,len-1);
		return str[0]==str[len-1]? len<=2 || string_isPalindrome(str+1,len-2) : false;
	}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// Other functions for testing below
//

/**
 * Fill the string with specified character until it reaches a null char
 *
 * @param str String
 * @param c Specified character
 */
void string_fill_untilNull(char* str,char c){
	while(*str!='\0')
		*(str++)=c;//Set current char to the specified char and then increase position
}

/**
 * Fill the whole string with specified character
 *
 * @param str String
 * @param c Specified character
 * @param len Length of string
 */
void string_fill(char* str,char c,unsigned long len){
	while(len>0)
		str[--len]=c;//Decrement length, set current char (position: the decremented length) to the specified char
}

///////////////////////////////////////////////////////////////////////////////////////////////////////
// Main function. Testing all the functions
//

int main(int argc, char const *argv[]){
	int i;
	puts("Factorial:");
	for(i=0;i<=20;i++)
		printf("%02i! = %lli\n",i,factorial(i));
	putc('\n',stdout);

	char str[]="A B C D E F G H I J K L M N O P Q R S T U V W X Y Z \0";
	printf("Original string:\n%s\n\n",str);

	unsigned long len=strlen(str);//Length not including null char
	char strOut[len+1];//Allocate output string on stack
	string_fill(strOut,'\0',len+1);

	string_fill(strOut,'\0',len+1);
	printf("string_reverse1:\n%s\n\n",string_reverse1(str,len,strOut));

	string_fill(strOut,'\0',len+1);
	string_reverse2(str,len,strOut);
	printf("string_reverse2:\n%s\n\n",strOut);

	string_fill(strOut,'\0',len+1);
	string_reverse3(str,strOut);
	printf("string_reverse3:\n%s\n\n",strOut);

	printf("string_count_char1: %i\n",string_count_char1(str,' '));
	printf("string_count_char2: %i\n",string_count_char2(str,' '));
	printf("string_count_char3: %i\n",string_count_char3(str,' '));
	printf("string_count_whitespace: %i\n\n",string_count_whitespace(str));

	#define PALINDROME_STR "racecar"
	unsigned long palindrome_strLen=strlen(PALINDROME_STR);
	printf("string_isPalindrome("PALINDROME_STR"): %i\n",string_isPalindrome(PALINDROME_STR,palindrome_strLen));

	return 0;
}
