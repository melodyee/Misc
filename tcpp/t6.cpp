/*
 * t6.cpp
 *
 *  Created on: 2011-2-25
 *      Author: zsc
 */

#include<iostream>

using namespace std;

//class LoserTree {
//	int data;
//	LoserTree *left;
//	LoserTree *right;
//public:
//	LoserTree(int *a,unsigned alen);
//};
//
//LoserTree::LoserTree(int *a,unsigned alen) {
//	if(alen==0)
//		throw "Empty array";
//
//}

class List {
	int v;
public:
	List (int _v) : v(_v), next(0) {}
	List *next;
	List *cons (int _v);
	unsigned loop_length();
	void display() {
		List *p = this;
		while(p) {
			cout << p->v << " ";
			p = p->next;
		}
		cout << endl;
	}
};

List *List::cons(int _v) {
	List *l = new List(_v);
	l -> next = this;
	return l;
}

unsigned List::loop_length() {
	List *p = this;
	List *p2 = this;
	unsigned len = 0;
	while (p) {
		len ++;
		p = p->next;
		p2 = p2->next;
		if (!p2) {
			return 0;
		}
		else {
			p2 = p2->next;
			if (p==p2)
				return len;
			if (!p2)
				return 0;
		}
	}
	return 0;
}

class Trie {
public:
	string data;
	map<string,Trie*> m;
	void insert(char *s) {

	}
};

int main () {
	List *p = new List(0);
	List *l = p;
	l = l->cons(1)->cons(2)->cons(3);
	l->display();
	cout << l->loop_length() << endl;

	p->next = l->next;
	cout << l->loop_length() << endl;

//	l->display();
	return 0;
}
