#include <iostream>
#include <list>
#include <cstdlib>

using namespace std;//The only thing we are gonna use are the standard library stuff

/**
 * Prints a list of ints to cout
 */
template <typename T>
void print_list(list<T> l){
	cout<<'[';
	for(typename list<T>::iterator i=l.begin();i!=l.end();i++)
		cout<<*i<<',';
	cout<<']'<<endl;
}

/**
 * A quicksort algorithm implemented mutably and recursively where performance and
 * memory usage is ignored. No optimization is thought of.
 * An implementation that "JUST WORKS" because it was promised that performance didn't matter.
 * Also, no requirements for a type generic version or sort stability.
 * Only readability was required. Remember the promise. // Edwin P, 2013-09-23
 *
 * If you want a better version for grading, contact me and don't use this one.
 */
template <typename T>
void quicksort(list<T>& l){
	size_t size=l.size();

	//1: If the number of elements in list is 0 or 1, return
	if(size<=1)
		return;

	//2: Pick pivot element (Choosing an element from a random position)
	typename list<T>::iterator pivot_iterator=l.begin();
	advance(pivot_iterator,rand()%size);
	T pivot=*pivot_iterator;
	l.erase(pivot_iterator);

	//3: Partition elements based on greater than pivot and lesser than pivot
	list<T> left,right;

	do{//Loop through every element, inserting elememt<=pivot to the left list and element>pivot to the right list 
		if(l.front()>pivot)
			right.push_front(l.front());
		else
			left.push_front(l.front());

		l.pop_front();//Remove element for next one in the loop
	}while(!l.empty());

	//Sort the left and right list recursively
	quicksort(left);
	quicksort(right);

	/////////////////////////////////////////
	// Assemble everything together in order
	// (l is now empty)

	//Add pivot
	l.push_back(pivot);

	//Add left partition at the beginning
	l.splice(l.begin(),left);

	//Add right partition at the end
	l.splice(l.end(),right);
}

/**
 * A mergesort algorithm implemented mutably and kind of recursively
 */
template <typename T>
void mergesort(list<T>& l){
	//If size of list <= 1, then it's already sorted
	if(l.size()<=1)
		return;

	//Make an iterator and advance to the middle of the list for splitting
	//The iterator will point to the middle of the list
	typename list<T>::iterator middle_iterator=l.begin();
	advance(middle_iterator,l.size()/2);

	//The left part of the half (a) and the right part of the half (b)
	list<T> a,b;
	a.splice(a.begin(),l,middle_iterator,l.end());//Move right part of the half to b
	b.splice(b.begin(),l);//Move left part of the half to a

	//Call function recursively until all the elements are splitted to one element
	mergesort(a);
	mergesort(b);

	//Move all elements from a and b to l until one of the lists is empty, sorting it iteratively in the progress
	do{
		//If head of a < head of b
		if(a.front()<b.front()){
			//Move head of a
			l.push_back(a.front());
			a.pop_front();
		}else{
			//Move head of b
			l.push_back(b.front());
			b.pop_front();
		}
	}while(!a.empty() && !b.empty());

	//When one of the lists becomes empty, then there could be elements left in one of the splitted (and sorted) lists, now containing the greatest values
	//If a is not empty, move the rest from a to l
	while(!a.empty()){
		l.push_back(a.front());
		a.pop_front();
	}

	//If b is not empty, move the rest from b to l
	while(!b.empty()){
		l.push_back(b.front());
		b.pop_front();
	}
}

int main(int argc,const char* argv[]){
	int array[]={1,3,5,2,4};
	list<int> l1(array,array+sizeof(array)/sizeof(int));
	list<int> l2(array,array+sizeof(array)/sizeof(int));

	cout<<"List     : ";print_list(l1);

	quicksort(l1);
	cout<<"quicksort: ";print_list(l1);

	mergesort(l2);
	cout<<"mergesort: ";print_list(l2);

	return 0;
}
