#include <stdio.h>

int sum(int n){
	int nummer = 0;

	while(n>0){
		nummer+=n;
		n-=1;
	}
	return nummer;
}

int sumr(int n){
	if(n>0)
		return n+sumr(n-1);
	else
		return n;
}

int main(){
	printf("%d\n",sum(5));
	printf("%d\n",sumr(5));

	return 0;
}