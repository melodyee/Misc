/*
 * t7.cpp
 *
 *  Created on: 2011-2-27
 *      Author: zsc
 */

#include<iostream>
#include<cstdio>
#include<hash_map>
#include<map>
#include<queue>
#include <cstring>

using namespace std;
using namespace __gnu_cxx;

class Tree {
public:
	int data;
	Tree *left;
	Tree *right;
	Tree(int _data,Tree *_left=0,Tree *_right=0) : data(_data), left(_left),right(_right) {}
};
bool contains(Tree*t, Tree*n) {
	if (t==n) return true;
	if(!t) return false;
	return contains(t->left,n) || contains(t->right,n);
}
bool lca(Tree *t, Tree*n1, Tree *n2, Tree *&found) {
	if (!t) return false;
	if (t==n1) {
		if(contains(n1,n2)) {
			found =n1;
			return true;
		}
		return false;
	}
	if (t==n2) {
		if(contains(n2,n1)) {
			found =n2;
			return true;
		}
		return false;
	}
	if (lca(t->left,n1,n2,found))
		return true;
	if (lca(t->right,n1,n2,found))
		return true;
	found = t;
	return contains(t,n1) && contains(t,n2);
}
enum dir { d_n, d_up, d_left, d_leftup};
template <class T> T** alloc2(unsigned m,unsigned n) {
	T **r = new T*[m];
	for(unsigned i=0;i<m;i++) {
		r[i] = new T[n];
		for(unsigned j=0;j<n;j++)
			r[i][j] = T(0);
	}
	return r;
}
template <class T> void dealloc2(T** a, unsigned m) {
	for(unsigned i=0;i<m;i++)
		delete [] a[i];
	delete [] a;
}
int lcs(char *s1,char* s2) {
	if(!s1 || !s2) return 0;
	unsigned res = 0;
	unsigned len1 = strlen(s1);
	unsigned len2 = strlen(s2);
	if (!len1 || !len2) return 0;
	unsigned **a=alloc2<unsigned>(len1,len2);
	dir **dirs = alloc2<dir>(len1,len2);
	for(unsigned i=0;i<len1;i++)
		for(unsigned j=0;j<len2;j++) {
			if (i==0||j==0) {
				if (s1[i]==s2[j]) {
					a[i][j] = 1;
					dirs[i][j] = d_leftup;
				} else if (i==0) {
					a[i][j] = a[i][j-1];
					dirs[i][j] = d_left;
				} else {
					a[i][j] = a[i-1][j];
					dirs[i][j] = d_up;
				}
			} else if (s1[i] == s2[j]) {
				a[i][j] = a[i-1][j-1] + 1;
				dirs[i][j] = d_leftup;
			} else if (a[i-1][j] > a[i][j-1]) {
				a[i][j] = a[i-1][j];
				dirs[i][j] = d_up;
			} else {
				a[i][j] = a[i][j-1];
				dirs[i][j] = d_left;
			}
		}
	res = a[len1-1][len2-1];
	pair<int,int> p(len1-1,len2-1);
	while(1) {
		switch (dirs[p.first][p.second]) {
		case d_leftup:
			cout << s1[p.first] << " ";
			--p.first;--p.second;
			break;
		case d_left:
			--p.second;
			break;
		case d_up:
			--p.first;
			break;
		case d_n:
			p.first = p.second = 0;
			break;
		}
		if (p.second <0 || p.first <0)
			break;
	}
	cout << endl;
	dealloc2(dirs,len1);
	dealloc2(a,len1);
	return res;
}

class losertree {
	queue<int>** qs;
	unsigned n; //assume n is 2's power
	class loserpair {
	public:
		unsigned id;
		int val;
		loserpair(unsigned _id,int _val) : id(_id), val(_val) {}
		bool operator<(const loserpair& p2) const {
			return val<p2.val;
		}
	};
	vector<loserpair> a;
	int winner;
public:
	losertree (queue<int>** _streams, unsigned _n) : qs(_streams), n(_n) {
		a.resize(n-1,loserpair(0,0));
		vector<loserpair> b(2*n-1,loserpair(0,0));
		for(unsigned i=0;i<n;i++) {
			b[n-1+i] = loserpair(i,qs[i]->front());
			qs[i]->pop();
		}
		for(int i=n-2;i>=0;--i) {
			b[i] = min(b[2*i+1],b[2*i+2]);
			unsigned loser = b[2*i+2]<b[2*i+1]?b[2*i+1].id:b[2*i+2].id;
			a[i] = b[n-1+loser];
		}
		winner = b[0].val;
	}
	void print() {
		for(unsigned i=0;i<n-1;i++) {
			cout << a[i].id << " " << a[i].val << ",";
		}
		cout << endl;
	}
	int pop() {
		// TODO:wrong
		int r = winner;
		winner = a[0].val;
		unsigned i = 0;
		while(true) {
			unsigned i1 = 2*i+1;
			unsigned i2 = 2*i+2;
			if (i1>n-1 || i2>n-1)
				break;
			if (a[i1]<a[i2]) {
				a[i] = a[i1];
				i = i1;
			} else {
				a[i] = a[i2];
				i= i2;
			}
		}
		return r;
	}
};

struct eqstr {
	bool operator()(const char*s1, const char* s2) const {
		return strcmp(s1,s2)==0;
	}
};

void rot(pair<int,int>& p) {
	int a = p.first;
	int b = p.second;
	p.first = b;
	p.second = -a;
}

void add(pair<int,int>& p, const pair<int,int> d) {
	p.first += d.first;
	p.second += d.second;
}

void print_matrix(int **a, unsigned m, unsigned n) {
	if (m==0 || n==0) return;
	static pair<int,int> d = make_pair(0,1);
	static pair<int,int> p = make_pair(0,0);

	for(unsigned i=0;i<n;i++) {
//		cout << p.first << " " << p.second << endl;
		cout << a[p.first][p.second] << " ";
		if(i<n-1) add(p,d);
	}
	rot(d);
	add(p,d);
	print_matrix(a,n,m-1);
}

int ugly(unsigned n) {
	int* uglies = new int[n+1];
	uglies[0] = 1;
	int* p2 = uglies;
	int* p3 = uglies;
	int* p5 = uglies;

	unsigned i = 0;
	while(i<n) {
		while (*p2 * 2 <= uglies[i])
			++p2;
		while (*p3 * 3 <= uglies[i])
			++p3;
		while (*p5 * 5 <= uglies[i])
			++p5;
		uglies[++i] = min(min(*p2 * 2,*p3 * 3),*p5 *5);
	}
	int r = uglies[n];
	delete [] uglies;
	return r;
}

unsigned sym_len(const char *p) {
	if (!p) return 0; // maybe -1
	unsigned mlen = 0;
//	unsigned len = strlen(p);
	const char* l;
	const char* r;
	const char* cur = p;
	while (*cur) {
		l=r=cur;
		unsigned cnt = 0;
		while(l>=p && *r && *l==*r) {
			if (l==r)
				cnt+=1;
			else
				cnt+=2;
			--l;++r;
		}
		if(cnt>mlen)
			mlen = cnt;
		l=cur-1;
		r=cur;
		cnt = 0;
		while(l>=p && *r && *l==*r) {
			--l;++r;
			cnt += 2;
		}
		if(cnt>mlen)
			mlen = cnt;
		++cur;
	}
	return mlen;
}

class CNode {
public:
	int data;
	CNode *next;
	CNode *sibling;
	CNode (int _data, CNode *_next = 0, CNode *_sibling = 0) : data(_data), next(_next), sibling(_sibling) {}
	CNode *copy();
	void print();
};

void CNode::print() {
	CNode *p = this;
	while(p) {
		cout << p->data << " ";
		if (p->sibling)
			cout << "(" << p->sibling->data << ") ";
		p = p->next;
	}
	cout << endl;
}

CNode *CNode::copy() {
	CNode *n,*p;
	map<CNode*,CNode* > m;
	m[0] = 0;
	p = this;
	while (p) {
		n = new CNode (p->data,0,0);
		m[p] = n;
		p = p->next;
	}
	p = this;
	while (p) {
		n = m[p];
		n->next = m[p->next];
		n->sibling= m[p->sibling];
		p=p->next;
	}
	return m[this];
}

int main() {
	hash_map<const char*, int ,hash<const char*>, eqstr > hm;
	hm["a"] = 1;
	cout << hm["a"] << endl;
	cout << hm["b"] << endl;

	int **a = new int*[3];
	for(unsigned i=0;i<3;i++) {
		a[i] = new int[3];
		for(unsigned j=0;j<3;j++)
			a[i][j] = 3*i+j;
	}
	print_matrix(a,3,3);
	cout << endl;
	for(unsigned i=0;i<3;i++)
		delete [] a[i];
	delete [] a;

	cout << ugly(1500) << endl;

	cout << sym_len("") << endl;
	cout << sym_len("abc") << endl;
	cout << sym_len("abcb") << endl;
	cout << sym_len("abccb") << endl;

	CNode cn1(1),cn2(2),cn3(3);
	cn1.next = &cn2;
	cn2.next = &cn3;
	cn1.sibling = &cn2;
	cn2.sibling = &cn1;
	cn3.sibling = &cn2;
	cn1.print();
	cn1.copy()->print();

	Tree t1(1),t2(2),t3(3),t4(4);
	t1.left = &t2;
	t1.right = &t3;
	t3.left = &t4;

	Tree *found;
	if (lca(&t1,&t3,&t4,found))
		cout << found->data << endl;
	if (lca(&t1,&t3,&t2,found))
		cout << found->data << endl;

	cout << "lcs" << endl;
	cout << lcs("abcd","ad") << endl;
//	cout << LCS("abcd","ad") << endl;

	queue<int> q1,q2,q3,q4,q5,q6,q7,q8;
	q1.push(87);q1.push(99);q1.push(104);q1.push(119);
	q2.push(48);q2.push(56);q2.push(88);q2.push(97);
	q3.push(98);q3.push(104);q3.push(128);q3.push(151);
	q4.push(58);q4.push(70);q4.push(76);q4.push(100);
	q5.push(33);q5.push(91);q5.push(156);q5.push(205);
	q6.push(48);q6.push(55);q6.push(60);q6.push(68);
	q7.push(44);q7.push(55);q7.push(66);q7.push(77);
	q8.push(80);q8.push(96);q8.push(106);q8.push(113);
	queue<int>* qs[8] = {&q1,&q2,&q3,&q4,&q5,&q6,&q7,&q8};

	losertree lt = losertree(qs,8);
	lt.print();
	lt.pop();
	lt.print();
	return 0;
}
