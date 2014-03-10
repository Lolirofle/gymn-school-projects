//Euklides algoritm 2013-09-09

#include <stdio.h>

/**
 * Finds the greatest common divisor by using euclideans algorithm (Recursive version)
 *
 * http://sv.wikipedia.org/wiki/Euklides_algoritm
 *   1: Två heltal a och b, där a > b är givna.
 *   2: Om b = 0 är algoritmen klar och svaret är a.
 *   3: I annat fall beräknas c, resten när man delat a med b.
 *   4: sätt a = b, b = c och börja om från steg 2 igen.
 * 
 * @param a
 * @param b
 * @return The greatest common divisor for a and b
 */
int sgd(int a,int b){
	if(b==0)
		return a;
	return sgd(b,a%b);}

int main(int argc,char const *argv[]){
	#define A 1029
	#define B 1071

	int div=sgd(A,B);
	printf("SGD(%i,%i) = %i\n",A,B,div);
	printf("%i/%i = %i/%i\n",A,B,A/div,B/div);
	return 0;
}
