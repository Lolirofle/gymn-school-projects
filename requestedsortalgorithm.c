#include <stdio.h>

void sort(int* array,size_t length){
	if(length!=2){
		const size_t lengthHalf=length/2;
		sort(array,lengthHalf);
		sort(array+lengthHalf,lengthHalf);
	}
	if(array[0]>array[1]){
		int tmp=array[0];
		array[0]=array[1];
		array[1]=tmp;
	}
}

int main(int argc, char const *argv[]){
	int array[8]={7,6,5,4,3,2,1,0};
	sort(array,8);
	for(size_t i=0;i<8;++i)
		printf("%i ",array[i]);
	return 0;
}