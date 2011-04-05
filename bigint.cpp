/*
 * bigint.cpp
 *
 *  Created on: 2011-4-5
 *      Author: joe
 */

#include<vector>
#include<iostream>
using namespace std;

class BigIntRadix {
	vector<unsigned char> a; // little endian
	unsigned radix;
	char i2c(const unsigned char i) const {
		if (i<10) return '0'+i;
		if (i<36) return 'A'+(i-10);
		cerr << i << endl;
		throw "i2c";
	}
	unsigned char c2i(const char c) const {
		if (c>='0' && c<='9') return c-'0';
		if (c>='A' && c<='Z') return c-'A'+10;
		cerr << c << endl;
		throw "c2i";
	}
public:
	BigIntRadix(const vector<unsigned char>& _a, unsigned _radix)
   	   :a(_a), radix(_radix){}
   void print() const; // print as decimal
   unsigned div(int divisor); // returns the residue
   bool isZero() const; // can keep a flag to make it O(1)
   void convert(int newRadix);
};

void BigIntRadix::convert(int newRadix) {
	vector<unsigned char> b;
	while (!isZero()) {
		b.push_back(div(newRadix));
	}
	a = b;
	radix = newRadix;
}

bool BigIntRadix::isZero() const{
	for(vector<unsigned char>::const_iterator it = a.begin();it!=a.end();++it) {
		if (*it != 0) return false;
	}
	return true;
}

unsigned BigIntRadix::div(int n) {
	if (n<=0) throw "BigInt::div n <=0";
    unsigned alen = a.size();
    unsigned r = 0;
    for(int i = alen-1;i>=0;--i) {
    	unsigned t = (radix*r+a[i])/n;
    	r = (radix*r+a[i]) - t*n;
    	a[i] = t;
    }
    return r;
}

// TODO: overall O(n^2), may re-use some intermediate results?
// print as 10-radix, little endian
void BigIntRadix::print() const {
	int n = a.size();
	bool leading = true;
	for(int i=n-1;i>=0;i--) {
		if (leading && !a[i]) continue;
		leading = false;
		cout << i2c(a[i]);
	}
	cout << endl;
}

int main() {
	unsigned char cs[] = {1,2,0,1};
	vector<unsigned char> v(cs,cs+4);
	BigIntRadix bi(v,3);
	bi.convert(2);
	bi.print();
	bi.convert(16);
	bi.print();
	return 0;
}
