/*
 * t3.cpp
 *
 *  Created on: 2011-2-25
 *      Author: zsc
 */
#include<iostream>
#include<cstdlib>
#include <algorithm>
#include <vector>
#include <set>

using namespace std;
int med(const int* a,unsigned alen) {
	if (alen==0)
		throw "Empty array";
	return a[alen/2];
}

int binIndex(int x, const int* a, unsigned n) {
	int left=0;
	int right=n-1;
	while(left<=right){
		int middle=(left+right)/2;
		if (x==a[middle]) return middle;
		if (x>a[middle]) left=middle+1;
		else right=middle-1;
	}
	return -1;
}

int numStrictBelow(int x,const int* a,unsigned n) {
	int left=0;
	int right=n-1;
	int middle=-1;
	while(left<=right){
		middle=(left+right)/2;
		if (x==a[middle]) {
			while(x==a[middle] && middle >=0) {
				middle--;
			}
			return middle+1;
		}
		if (x>a[middle]) left=middle+1;
		else right=middle-1;
	}
	while(x<=a[middle] && middle >=0) {
		middle--;
	}
	return middle+1;
}

int kth(unsigned k, int *a, unsigned alen) {
	if (k>=alen)
		throw "Out of bound";
	return a[k];
}

//int kth2(unsigned k, int* a,int alen, int* b,int blen) {
//	if (alen==0) return kth(k,b,blen);
//	if (blen==0) return kth(k,a,alen);
//
//	int a1 = alen/2;
//	int a2 = alen - a1;
//	int b1 = blen/2;
//	int b2 = blen -b1;
//
//	if(2*(a1+a2) == alen+blen)
//}

int special(int x,const int *b, unsigned blen) {
	if (blen%2==0) {
		if (x<b[blen/2])
			return max(x,b[blen/2-1]);
		else
			return b[blen/2];
	} else {
		if (x<=b[blen/2])
			return b[blen/2];
		else {
			if (blen==1) return x;
			return min(x,b[blen/2+1]);
		}
	}
}

int med2(const int *a, unsigned alen, const int *b, unsigned blen) {
	if(alen==0) return med(b,blen);
	if(blen==0) return med(a,alen);
	if(alen==1) return special(a[0],b,blen);
	if(blen==1) return special(b[0],a,alen);

	int a1 = alen/2;
	int a2 = alen - a1;
	int b1 = blen/2;
	int b2 = blen - b1;

	if(a[a1] <= b[b1]) {
		int len = min(a1,b2);
		return med2(a+len,alen-len,b,blen-len);
	} else {
		int len = min(a2,b1);
		return med2(b+len,blen-len,a,alen-len);
	}
}

int* merge(const int *a,unsigned alen, const int* b, unsigned blen) {
	int *r = new int[alen+blen];
	unsigned i1 = 0;
	unsigned i2 = 0;
	unsigned i3 = 0;
	while (i1<alen && i2 <blen) {
		if (a[i1] <=b[i2]) {
			r[i3++] = a[i1++];
		} else {
			r[i3++] = b[i2++];
		}
	}
	while(i1<alen)
		r[i3++]=a[i1++];
	while(i2<blen)
		r[i3++]=b[i2++];
	return r;
}

pair<int *,int> uniq(const int* a,unsigned alen) {
	set<int> s(a,a+alen);
	int n = s.size();
	int *r = new int[n];
	int cnt = 0;
	for(set<int>::iterator it = s.begin();it!=s.end();++it)
		r[cnt++] = *it;
	return make_pair(r,cnt);
}

void checkMed2 (const int *a,unsigned alen, const int* b, unsigned blen) {
	int *r = merge(a,alen,b,blen);
	if(med(r,alen+blen) != med2(a,alen,b,blen)) {
		throw "checkMed2 failed";
	}
}

void testMed2() {
	{
		int a[] = {1,7,9,10,30};
		int b[] = {3,5,8,11};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}

	{
		int a[] = {1};
		int b[] = {};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}
	{
		int a[] = {1};
		int b[] = {2};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}

	{
		int a[] = {};
		int b[] = {2};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}

	{
		int a[] = {1,3,9};
		int b[] = {2};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}

	{
		int a[] = {1,3};
		int b[] = {2,8};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}

	{
		int a[] = {1,3,9,13};
		int b[] = {2,5,9};
		checkMed2(a,sizeof(a)/sizeof(int),b,sizeof(b)/sizeof(int));
	}


}

typedef struct list {
	int data;
	struct list * next;
} list;

list *cons(int e,list* l) {
	list *p = new list;
	p -> next = l;
	p -> data = e;
	return p;
}

void print (list *l) {
	while (l) {
		cout << l->data;
		l = l->next;
		if (l)
			cout << " ";
	}
	cout << endl;
}

int* reservoir (list *l, int k) {
	int* res = new int[k];
	int cnt = 0;
	while (l) {
		if(cnt <k) {
			res[cnt] = l->data;
		} else {
			int i = rand()%(cnt+1);
			if (i<k)
				res[i] = l->data;
		}
		l = l->next;
		cnt ++;
	}
	if (cnt <k) {
		delete [] res;
		return 0;
	}
	return res;
}

void print_array(const int *a, unsigned len) {
	for (unsigned i=0;i<len;i++) {
		cout << a[i] << " ";
	}
	cout << endl;
}

int* partialProduct(int *a,unsigned len) {
	int *res = new int[len];
	int s = 1;
	for(unsigned i = 0;i<len;i++) {
		res[i] = s;
		s *= a[i];
	}
	return res;
}

int* partialProductBackward(int *a,unsigned len) {
	int *res = new int[len];
	int s = 1;
	for(int i = len-1;i>=0;i--) {
		res[i] = s;
		s *= a[i];
	}
	return res;
}

int* prob(int *a, unsigned len) {
	int *b = partialProduct(a, len);
	int *c = partialProductBackward(a, len);
	for(unsigned i=0;i<len;i++) {
		c[i] *= b[i];
	}
	delete [] b;
	return c;
}

void qsort(int* a, int left,int right) {
	int l = left;
	int r = right;
	int p = (l+r)/2;
	if (l<r) {
		while (l <= p && p <= r) {
			while (l <= p && a[l] < a[p])
				l++;
			while (r >= p && a[r] > a[p])
				r--;
			int t = a[l];
			a[l] = a[r];
			a[r] = t;
			l++;
			r--;
			if (l-1==p)
				p = r = r+1;
			else if (r+1==p)
				p= l = l-1;
		}
		qsort(a,left,p-1);
		qsort(a,p+1,right);
	}
}

void swap(int& a, int& b) {
	int t = a;
	a = b;
	b = t;
}

void _kSubset(int k, const int* a, int alen, vector<int>& stack) {
	if(k>alen || k<0 || alen<0) return;
	if(k==alen) {
		unsigned cnt = stack.size();
		for (unsigned i=0;i<cnt;i++)
			cout << stack.at(i) << " ";
		print_array(a,alen);
		return ;
	}
	_kSubset(k,a+1,alen-1,stack);
	stack.push_back(a[0]);
	_kSubset(k-1,a+1,alen-1,stack);
	stack.pop_back();
}

void kSubset(unsigned k, const int* a, unsigned alen) {
	vector<int> stack;
	_kSubset(k,a,alen,stack);
}

//vector<vector<int> >& kSubset(unsigned k, vector<int>& a) {
//	unsigned sz = a.size();
//	vector<vector<int> >* r = new vector<vector<int> >;
//	if (k > sz) {
//		return *r;
//	}
//	else if (k==sz) {
//		r->push_back(a);
//		return *r;
//	}
//	for(unsigned i=0;i<sz;i++) {
//		vector<int>b (a);
//		b.erase(b.begin()+i);
//		vector<vector<int> > res = kSubset(k,b);
//		for(vector<vector<int> >::iterator it = res.begin();it!=res.end();++it )
//			r->push_back(*it);
//	}
//	return *r;
//}
//
//void testKSubset(unsigned k, const int* a, unsigned alen) {
//	vector<int> v(a,a+alen);
//	vector<vector<int> > res = kSubset(k,v);
//	for(vector<vector<int> >::iterator it = res.begin();it!=res.end();++it) {
//		vector<int> vi = *it;
//		for(vector<int>::iterator vit = vi.begin();vit!=vi.end();++vit)
//			cout << *vit << " ";
//		cout << endl;
//	}
//}

//int *order(int budget, int* r, int *o, unsigned len) {
//	int *r = new int[len];
//
//}

class Graph {
	unsigned n; // node from 1 to n, with a[i][j] = 1 for having a connection
	int **a;
public:
	Graph (unsigned _n);
	~Graph();
	void dfs();
};

Graph::Graph (unsigned _n) {
	n = _n;
	a = new int* [n];
	for(unsigned i=0;i<n;i++)
		a[i] = new int[n];
}
Graph::~Graph() {
	for(unsigned i = 0;i<n;i++)
		delete [] a[i];
	delete [] a;
}

int main() {
	int a[] = {1,7,9,10,30};
	int b[] = {3,5,8,11};
	int d[] = {1,2,3,4};

//	cout << numStrictBelow(3,a,5) << endl;
	cout << med2 (a,5,b,4)<<endl;
	int *m = merge(a,5,b,4);
	print_array(m,9);
	checkMed2(a,5,b,4);

	testMed2();

	cout << binIndex(1, a , sizeof(a)/sizeof(int)) <<endl;

	list *l = cons(1,cons(2,cons(3,cons(4,0))));
	srand(time(0));
	print_array(reservoir(l,2), 2);

	int *c = prob(d,4);
	print_array(c,4);

	int e[] = {3,8,2,4};
	vector<int> myv(e,e+4);
	qsort(e,0,3);
	print_array(e,4);

	sort(myv.begin(),myv.end());
	for(vector<int>::iterator it = myv.begin();it!=myv.end();++it)
		cout << *it << " ";
	cout << endl;

	int f[] = {1,2,3,2,1,5};
	pair<int *,int> p = uniq(f,6);
	cout << p.second << endl;
	print_array(p.first,p.second);

	int g[] = {1,2,3,4,5,6,7,8,9,10};
//	testKSubset(2,g,4);
	cout << "kSubset" << endl;
	kSubset(2,g,10);
}
