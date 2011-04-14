/*
 * t5.cpp
 *
 *  Created on: 2011-2-25
 *      Author: zsc
 */

#include<iostream>
#include<fstream>
#include<sstream>
#include<vector>
#include<algorithm>

using namespace std;

int toInt(string line) {
	istringstream s(line);
	int in;
	s >> in;
	return in;
}

void sortf(char* fname) {
	ifstream inf(fname);
	vector<int> v;
	string line;
	if (inf.is_open()) {
		while (inf.good()) {
			getline(inf, line);
			istringstream s(line);
			int in;
			s >> in;
			v.push_back(in);
		}
		inf.close();
	}
	sort(v.begin(),v.end());

	ofstream outf(fname);
	for(vector<int>::iterator it = v.begin();it!=v.end();++it)
		outf << *it << endl;
	outf.close();
}

void twoMerge(ifstream& a, ifstream& b, ofstream& os) {
	if (!a.is_open() || !b.is_open())
		throw "closed stream";
	string linea,lineb;
	int ia,ib;
	int goa = 1;
	int gob = 1;
	while (a.good() && b.good()) {
		if (goa) {
			getline(a,linea);
			ia = toInt(linea);
		}
		if (gob) {
			getline(b,lineb);
			ib = toInt(lineb);
		}
//		cout << ia << " " << ib << endl;
		if (ia<ib) {
			os << ia << endl;
			goa = 1;
			gob = 0;
		} else {
			os << ib << endl;
			goa = 0;
			gob = 1;
		}
	}
	while (a.good()) {
		getline(a,linea);
		os << linea << endl;
	}
	while (b.good()) {
		getline(b,lineb);
		os << lineb << endl;
	}
}

//void kmerge(k,ifstream* a, unsigned alen, oftream& os) {
//	if (alen==0)
//		throw "no input stream";
//	if (alen==1) {
//		if (a[0].is_open()) {
//			string line;
//			while(a[0].good()) {
//				getline(a[0],line);
//				os << line << endl;
//			}
//		}
//		return ;
//	}
//}

int main() {
	sortf("f0");
	sortf("f1");
	ifstream f0("f0");
	ifstream f1("f1");
	ofstream f2("f2");
	twoMerge(f0,f1,f2);
	return 0;
}
