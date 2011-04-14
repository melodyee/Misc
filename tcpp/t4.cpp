/*
 * t4.cpp
 *
 *  Created on: 2011-2-25
 *      Author: zsc
 */

#include<iostream>
#include<fstream>
#include<string>
#include<sstream>
#include<algorithm>
#include<vector>
using namespace std;
int main() {
	unsigned n = 100;
	string line;
	ofstream* ofs = new ofstream[n];
	for (unsigned i = 0; i < n; i++) {
		ostringstream s;
		s << "bins/" << i ;
		ofs[i].open(s.str().c_str());
	}

	ifstream inf("big2");
	if (inf.is_open()) {
		while (inf.good()) {
			getline(inf, line);
			istringstream s(line);
			int in;
			s >> in;
			unsigned i = in % n;
			ofs[i] << line << endl;
		}
		inf.close();
	}
	for(unsigned i=0;i<n;i++) {
		ofs[i].close();
	}

	for(unsigned i=0;i<n;i++) {
		ostringstream s;
		s << "bins/" << i ;
		ifstream inf(s.str().c_str());
		vector<int> v;
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

		ofstream outf(s.str().c_str());
		for(vector<int>::iterator it = v.begin();it!=v.end();++it)
			outf << *it << endl;
		outf.close();
	}
	return 0;
}
