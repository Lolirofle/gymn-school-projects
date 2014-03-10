#include <stdio.h>
#include <math.h>

struct Triangle{
	unsigned int a;
	unsigned int b;
	unsigned int c;
};

struct Triangle getPythagorianTriple(unsigned int a,unsigned int b){
	return (struct Triangle){a*a-b*b,2*a*b,a*a+b*b};
}

int main(int argc,const char* argv[]){
	struct Triangle triple;
	for(unsigned int i=1;i<10;++i){
		for(unsigned int j=i+1;j<10;j+=2){
			if((j-i)%2==0)
				continue;

			triple=getPythagorianTriple(j,i);
			//printf("%i %i : %i %i %i\n",i,j,triple.a,triple.b,triple.c);
			printf("%i %i %i\n",triple.a,triple.b,triple.c);
		}
	}

	return 0;
}
