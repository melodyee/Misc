/*
 * t8.cpp
 *
 *  Created on: 2011-3-7
 *      Author: zsc
 */

#include<iostream>
#include<sstream>
#include<stack>
#include<list>
#include<map>
#include<set>
#include<cmath>
#include<vector>
#include<queue>
#include<algorithm>
#include<cctype>
#include<cstring>
#include<cassert>
#include<climits>
#include<string>
#include<bitset>

#include "fraction.h"

#define N 4
using namespace std;
bool maze[N*N] = {
		true, false, true, true,
		true, true, true, false,
		true, false, true, false,
		true, false, true, true
};
typedef pair<int,int> pos;
typedef pair<int,int> dir;

void next(const dir& d, dir& nd) {
	nd.first = d.second;
	nd.second = -d.first;
}

bool hasNext(const dir& d) {
	return !(d.first==-1 && d.second==0);
}

void findPath(bool *maze, unsigned n) {
	if (n==0) return;
	stack<pos> s;
	pos final = make_pair(n-1,n-1);
	bool *visited = new bool[n*n];
	dir *dirs = new dir[n*n];
	for(unsigned i=0;i<n*n;i++) {
		visited[i] = false;
		dirs[i] = make_pair(0,1);
	}
	s.push(make_pair(0,0));
	visited[0] = true;
	while (!s.empty()) {
		pos p = s.top();
//		cout << " (" << p.first << ","<< p.second << ")" << endl;
		if (p==final) {
			while(!s.empty()) {
				pos p = s.top();
				cout << "(" << p.first << ","<< p.second << ")" << " ";
				s.pop();
			}
			cout << endl;
			return;
		}
		unsigned idx = p.first*n+p.second;
		bool found = false;
		while (hasNext(dirs[idx])) {
			dir d (dirs[idx]);
			int i = p.first+d.first;
			int j = p.second+d.second;
			if (i>=0 && j>=0 && i<n && j<n && maze[i*n+j] && !visited[i*n+j]) {
				s.push(make_pair(i,j));
				visited[i*n+j] = true;
				next(d,dirs[idx]);
				found = true;
				break;
			}
			next(d,dirs[idx]);
		}
		if (!found)
			s.pop();
	}
	delete [] dirs;
	delete [] visited;
}

int comparePriority(const char c1,const char c2) {
	switch(c1) {
	case '+':
	case '-':
		switch(c2) {
		case '+': return 1;
		case '-':return 1;
		case '*':return -1;
		case '/':return -1;
		case '(':return -1;
		case ')':return 1;
		case '#':return 1;
		}
	case '*':
	case '/':
		switch(c2) {
		case '(':return -1;
		default: return 1;
		}
	case '(':
		switch(c2) {
		case ')': return 0;
		case '#':assert(false);
		default:return -1;
		}
	case ')':
		switch(c2) {
		case '(':assert(false);
		default:return 1;
		}
	case '#':
		switch(c2) {
		case '#':return 0;
		case ')':assert(false);
		default:return -1;
		}
	}
	assert(false);
}

int evalOp(char op, int i,int j) {
	int r;
	switch(op) {
	case '+':r = i+j; break;
	case '-':r = i-j; break;
	case '*':r = i*j; break;
	case '/':r = i/j; break;
	default:throw "evalOp error";
	}
	return r;
}

int eval(const char* s, unsigned slen) {
	if(!slen)return 0;
	stack<int>operands;
	stack<char>ops;
	ops.push('#');
	unsigned i=0;
	while(i<slen || ops.top()!='#') {
		char op = ops.top();
		char c=s[i];
		if(isdigit(c)) {
			operands.push(c-'0');
			++i;
		} else {
			switch (comparePriority(op,c)) {
			case -1:
				ops.push(c);
				++i;
				break;
			case 0:
				ops.pop();
				++i;
				break;
			case 1:
				ops.pop();
				int b=operands.top();
				operands.pop();
				int a=operands.top();
				operands.pop();
				operands.push(evalOp(op,a,b));
				break;
			}
		}
	}
	return operands.top();
}
bool myCmp(int i,int j) {
	return i>j;
}

//void printDiag(unsigned m, unsigned n) {
//	if (!m || !n) {
//		cout << endl;
//		return;
//	}
//	unsigned cnt = 0;
//	unsigned m1 = m*(m+1)/2;
//	unsigned m2 = m*n - m*(m+1)/2;
//	unsigned wrap = 1;
//	unsigned i = 0;
//	unsigned j = 0;
//	while(cnt <m*n) {
//		if(cnt<m1) {
//			unsigned i2 = i;
//			unsigned i2 = i;
//			for(unsigned k=0;k<wrap;k++) {
//				cout << "(" << i << "," << j << ")" << " ";
//				--i;
//				++j;
//			}
//			cout << endl;
//			++wrap;
//		} else if (cnt <m2) {
//
//		} else {
//
//		}
//		++cnt;
//	}
//}

//typedef list<pair<int,double> > idlist;
//void mulList(const idlist& l1, const idlist& l2, idlist& l3) {
//
//}

typedef map<int,double> idmap;
void mulMap(const idmap& m1, const idmap& m2, idmap& m3) {
	m3.clear();
	for(idmap::const_iterator it=m1.begin();it!=m1.end();++it) {
		for(idmap::const_iterator it2=m2.begin();it2!=m2.end();++it2) {
			m3[it->first+it2->first] += it->second*it2->second;
		}
	}
}

void calcProb(const double* p,unsigned plen) {
	idmap *ms=new idmap[plen];
	idmap res;
	res[0] = 1;
	for(unsigned i=0;i<plen;i++) {
		ms[i][0] = 1-p[i];
		ms[i][1] = p[i];
		idmap ores(res);
		mulMap(ores,ms[i],res);
	}
	for(unsigned i=0;i<plen+1;i++) {
		cout << i << "," << res[i] << endl;
	}
	delete [] ms;
}

void testCalcProb() {
	double ps[] = {0.2, 0.3, 0.0, 1.0, 0.8, 0.9};
	calcProb(ps,6);
}
typedef map<unsigned,vector<unsigned> > graph;
void topoSort(graph g, unsigned n) {
	map <unsigned,bool> poped;
	queue<unsigned> q;
	for(unsigned i=0;i<n;i++)
		q.push(i);
	while(!q.empty()) {
		unsigned t = q.front();
		if (g[t].empty()) {
			q.pop();
			poped[t] = true;
		} else {
			vector<unsigned> v = g[t];
			int s = 0;
			for(vector<unsigned>::iterator it = v.begin();it!=v.end();++it) {
				if (!poped[*it]) ++s;
			}
			if (s==0) {
				q.pop();
				poped[t] = true;
			}
		}
	}
}

int minTime(int n, vector<int> roadTime, vector<int> flightTime, int k) {
	vector<int> v;
	int s=0;
	for(int i =0;i<n;i++) {
		s+=roadTime[i];
		if (roadTime[i]-flightTime[i] >0)
				v.push_back(roadTime[i]-flightTime[i]);
	}
	sort(v.begin(),v.end());
	for(int i=0;i<k;i++) {
		if (v.empty()) break;
		s -= v.back();
		v.pop_back();
	}
	return s;
}

void outputOrder(const char* s,unsigned n) {
	graph g;

}

class SellingProducts {
public:
	int optimalPrice(vector <int> price, vector <int> cost) {
		vector<pair<int,int> > pcs;
		int n = price.size();
		for(int i=0;i<n;i++)
			pcs.push_back(make_pair(price[i],cost[i]));
		sort(pcs.begin(),pcs.end());
		int lv;
		int mlv =0;
		int mprofit = 0;
		for(int i=0;i<n;i++) {
			lv = pcs[i].first;
			int profit = 0;
			for(int j=0;j<n;j++) {
				if (pcs[j].first >= lv)
					profit += max(0,lv-pcs[j].second);
			}
			if (profit>mprofit) {
				mlv = lv;
				mprofit = profit;
			}
		}
		return mlv;
	}
	void test() {
		int ps[] = {13,17,14,30,19,17,55,16};
		int cs[] = {12,1,5,10,3,2,40,19};
		vector<int> vp(ps,ps+sizeof(ps)/sizeof(int));
		vector<int> vc(cs,cs+sizeof(cs)/sizeof(int));
		cout << optimalPrice(vp,vc) << endl;
	}
};

class JumpingBoard {
public:
	int maxLen(int i,int j, int dir, vector <string> board,
			int _m,int _n, map<pair<int,int>,bool > visited) {
		static map<pair<int,int>, int> memo;
		if (memo.find(make_pair(i,j))!=memo.end())
			return memo[make_pair(i,j)];
		int r;
		char c = board[i][j];
		int n = c-'0';
		int ni,nj;
		switch(dir) {
		case 0:ni=i;nj=j+n;break;
		case 1:ni=i+n;nj=j;break;
		case 2:ni=i;nj=j-n;break;
		case 3:ni=i-n;nj=j;break;
		}
		if (ni>=0 && ni<_m && nj>=0 && nj <_n && board[ni][nj]!='H') {
			if(visited[make_pair(ni,nj)])
				throw "inf";
			visited[make_pair(ni,nj)] = true;
			int ml=-1;
			for(int i=0;i<4;i++) {
				int l = maxLen(ni,nj,i,board,_m,_n,visited);
				if (l>ml){
					ml = l;
				}
			}
			memo[make_pair(ni,nj)] = ml;
			r= 1+ml;
		}
		else
			r = 1;
		return r;
	}
	int maxJumps(vector <string> board) {
		int m = board.size();
		if (!m) return 0;
		int n = board[0].length();
		map<pair<int,int>,bool > visited;
		int ml = -1;
		try {
			for(int i=0;i<4;i++) {
				int l = maxLen(0,0,i,board,m,n,visited);
				if (l>ml){
					ml = l;
				}
			}
			return ml;
		} catch(...) {
			return -1;
		}

	}
	void test() {
		string ss[] = {"3942178",		 "1234567",		 "9123532"};
		vector<string> board(ss,ss+3);
		cout << maxJumps(board) << endl;
	}
};

class RabbitNumbering {
public:
	int theCount(vector <int> maxNumber) {
		sort(maxNumber.begin(),maxNumber.end());
		long long r = 1;
		int len = maxNumber.size();
		for(int i=0;i<len;i++) {
//			cout << maxNumber[i] << " "<< i << endl;
			r = (r*(maxNumber[i]-i))%1000000007;
			if(r<=0) return 0;
		}
		return r;
	}
};

class Nisoku {
public:
	double theMax(vector <double> cards) {
		int n = cards.size();
		sort(cards.begin(),cards.end());
		double mp = 0;
		for(int s=0;s<=n;s+=2) {
			double p=1;
			for(int i=0;i<s/2;i++) {
				p *= (cards[i] + cards[s-1-i]);
			}
			for(int i=s;i<n;i++)
				p *= cards[i];
			if (p>mp) mp = p;
		}
		return mp;
	}
};

class TheBoredomDivOne {
	double g(int x,const double n, map<int,double>& G) {
		if (G.find(x)!=G.end())
			return G[x];
//		cout << "debug " << x << endl;
		double r;
		switch(x) {
		case 0:
			r= 0;
			break;
		case 1:
			r= g(0,n,G) + n/2.;
			break;
		default:
		{
			double p1 = x*(x-1.)/n/(n-1);
			double p2 = (n-x)*(n-1-x)/n/(n-1);
			double p3 = 1-p1-p2;
			r = (1+p1*g(x-2,n,G)+p3*g(x-1,n,G))/(1-p2);
			break;
		}
		}
		G[x] = r;
		return r;
	}
public:
	double find(int n, int m) {
		map<int,double> G;
		return g(m,n+m,G);
	}
};

int c(int m,int n) {
	static map<pair<int,int>,int > C;
	if(m>=n) return 0;
	if(m==n-1) return m;
	if (C.find(make_pair(m,n))!=C.end()) {
		return C[make_pair(m,n)];
	}
	int min_ = INT_MAX;
	for(int i=m;i<=n;i++) {
		if (i>=min_) continue;
		int c1 = c(i+1,n);
		if (i+c1>=min_) continue;
		int cur = i+max(c(m,i-1),c1);
		if (cur<min_) {
			min_ = cur;
		}
	}
	C[make_pair(m,n)] = min_;
	return min_;
}

int cs(int n) {
	int s=0;
	for(int i=0;i<=n;i++)
		s+=c(1,i);
	return s;
}

bool isPrime(int i) {
	if (i<=1) return false;
	if (i<=3) return true;
	if ((i&1)==0) return false;
	for(int j=3;j*j<=i;j+=2) {
		if (i%j==0) return false;
	}
	return true;
}

void primesBelow(int n, vector<int>& s) {
	s.clear();
	for(int i=0;i<n;i++) {
		if (isPrime(i))
			s.push_back(i);
	}
}

bool isSemiPrime (int i) {
	if (i<4) return false;
	for(int j=2;j*j<=i;j++) {
		if (!isPrime(j)) continue;
		if (i%j==0) {
			return isPrime(i/j);
		}
	}
	return false;
}

int coutSemiPrime (int i) {
	int s = 0;
	for(int j=0;j<i;j++) {
//		cout << j << endl;
		if (isSemiPrime(j)) {
//			cout << j << endl;
			s+=1;
		}
	}
	return s;
}

bool f(int i, set<int> ps) {
	if (i<4) return false;
	for(set<int>::iterator it = ps.begin();it!=ps.end();++it) {
		int p = *it;
		if (p*p>i) return false;
		if (i%p==0) {
			if (i==p*p) return true;
			if (i%(p*p)==0) return false;
			return isPrime(i/p);
		}
	}
	return false;
}

//int p187 (int i) {
//	set<int> ps;
//	primesBelow(int(sqrt(i)+1),ps);
//	int s = 0;
//	for(int j=0;j<i;j++) {
////		cout << j << endl;
//		if (f(j,ps)) {
//			cout << j << endl;
//			s+=1;
//		}
//	}
//	return s;
//}

int p187(int n, vector<int>& ps) {
	vector<int> s(n);
	for(vector<int>::iterator it = ps.begin();it!=ps.end();++it) {
		int p = *it;
		for(int i=p;i<n;i+=p)
			s[i]+=1;
	}
	for(vector<int>::iterator it = ps.begin();it!=ps.end();++it) {
		int p = *it;
		long long p2 = p*p;
		if (p2>n) break;
		s[p2] = 2;
		for(int i=2*p2;i<n;i+=p2)
			s[i]=0;
	}
	int cnt = 0;
	for(int i=0;i<n;i++)
		if (s[i]==2)
			cnt ++;
	return cnt;
}

typedef math::fraction<long long> fractionl;

void p329(int n,int m,const string& seq) {
//	math::fractioni f(4,5);
//	std::cout << (f + math::fractioni(3, 8) - 1)/math::fractioni(2, 3) << std::endl;

	vector<fractionl> ps(n);
	vector<fractionl> ps2(n);
	for(int i=0;i<n;i++)
		ps[i] = fractionl(1,n);
	fractionl p(1,1);
	for(int j=0;j<m;j++) {
		fractionl f(0,1);
		for(int i=0;i<n;i++) {
			if (isPrime(i+1) && seq[j] == 'P' || !isPrime(i+1) && seq[j] == 'N')
				f += ps[i] * fractionl(2,3);
			else
				f += ps[i] * fractionl(1,3);
			f.simplify();
		}
		for(int i=0;i<n;i++) {
			ps2[i] = fractionl(0,1);
			if(i>0)
				ps2[i] += ps[i-1] /2;
			if (i<n-1)
				ps2[i] += ps[i+1] /2;
		}
		for(int i=0;i<n;i++) {
			ps[i] = ps2[i];
		}
		cout << "f=" << f << endl;
		p *= f;
		p.simplify();
		cout << "p=" << p << endl;
	}

}

void sieve(int n, vector<int>& ps) {
	ps.clear();
	vector<char> vs(n);
	vs[0] = 1;
	vs[1] = 1;
	vector<int> small;
	primesBelow(int(sqrt(n)+1),small);
	for(vector<int>::iterator it = small.begin();it!=small.end();++it) {
		int p = *it;
		for(int i=2*p;i<n;i+=p) {
			vs[i] = 1;
		}
	}
	for(int i=0;i<n;i++) {
		if (vs[i]==0)
			ps.push_back(i);
	}
}

void sieve_odd(int _n, vector<int>& ps) {
	ps.clear();
	int n = (_n-1)/2;
	vector<char> vs(n);
	vector<int> small;
	sieve(int(sqrt(_n)+1),small);
	cout << "sieve done" << endl;
	int m = small.size();
	//skip 2
	for(int i=1;i<m;i++) {
		int p=small[i];
		for(int i=(3*p-1)/2;i<n;i+=p)
			vs[i] = 1;
	}
	for(int i=0;i<n;i++) {
		if (vs[i]==0)
			ps.push_back(1+(i<<1));
	}
}

void sieve_odd_bs(int _n, vector<int>& ps) {
	ps.clear();
	int n = (_n-1)/2;
	bitset<50000000> bs;
	vector<int> small;
	sieve(int(sqrt(_n)+1),small);
	cout << "sieve done" << endl;
	int m = small.size();
	//skip 2
	for(int i=1;i<m;i++) {
		int p=small[i];
		for(int i=(3*p-1)/2;i<n;i+=p)
			bs.set(i);
	}
	for(int i=0;i<n;i++) {
		if (bs.test(i))
			ps.push_back(1+(i<<1));
	}
}

bool next(vector<int>& v) {
	int n = v.size();
	for(int i=0;i<n;i++) {
		if (v[i]==0) {
			v[i]=1;
			return true;
		}
		else
			v[i] = 0;
	}
	return false;
}

void int2rv(int n, int radix, vector<int>& v) {
	v.clear();
	while (n) {
		v.push_back(n%radix);
		n/=radix;
	}
}

long long int rv2int(const vector<int>& v, int radix=10) {
	long long int s = 0;
	int n = v.size();
	int d = 1;
	for(int i=0;i<n;i++) {
		s += v[i]*d;
		d*=radix;
	}
	return s;
}

int the6Form8(int n) {
	vector<int> vn;
	int2rv(n,10,vn);

	vector<int> v(6);
	do {
		vector<int> can(vn);
		set<int> s;
		for(int i=0;i<10;i++) {
			if (i==0 && v[5]==1)continue;
			for(int j=0;j<6;j++) {
				if (v[j]==1)
					can[j] = i;
			}
//			cout << "can: ";
//			for(vector<int>::iterator it = can.begin();it!=can.end();++it)
//				cout << *it << " ";
//			cout << endl;
			int k = rv2int(can,10);
//			cout << isPrime(k);
			if (isPrime(k))
				s.insert(k);
		}
//		cout  << "#" << s.size() << endl;
		if (s.size()==8) {
			int min = INT_MAX;
			for(set<int>::iterator it = s.begin();it!=s.end();++it)
				if (*it < min)
					min = *it;
			return min;
		}

	} while(next(v));
	return -1;
}

//int a130(int n) {
//	cout << "a130 " << n << endl;
//	int cnt = 1;
//	vector<int> v;
//	v.push_back(1);
//	while (1) {
//		int k = rv2int(v,10);
//		if (k%n==0)
//			return cnt;
//		v.push_back(1);
//		cnt ++;
//	}
//	return -1;
//}

int a130(int n) {
//	cout << "a130 " << n << endl;
	int cnt =1;
	int s = 1;
	while(1) {
		if (s%n==0) {
//			cout << "done" << endl;
			return cnt;
		}
		s = (s*10+1)%n;
		cnt++;
	}
	return -1;
}

int f130(int n, vector<int>& ps) {
	ps.clear();
	vector<bool> vs(n);
	vs[0] = 1;
	vs[1] = 1;
	vector<int> small;
	primesBelow(int(sqrt(n)+1),small);
	for(vector<int>::iterator it = small.begin();it!=small.end();++it) {
		int p = *it;
		for(int i=2*p;i<n;i+=p) {
			vs[i] = 1;
		}
	}
	cout << "sieve done" << endl;
	int su = 0;
	for(int i=3;i<n;i++) {
//		cout << i << endl;
		if (vs[i]==1 && (i&1) && i%5!=0 && (i-1)%a130(i)==0) {
//			cout << i <<endl;
			su += i;
			ps.push_back(i);
			if (ps.size()==25)
				return su;
		}
	}
	return -1;
}

int p130() {
	vector<int> comp;
	return f130(1000000,comp);
}

bool only25(int n) {
	while((n&1)==0) {
		n>>=1;
	}
	while((n%5)==0)
		n/=5;
	return (n==0 || n==1);
}

int gcd (int a,int b) {
	if (a<0) a=-a;
	if (b<0) b=-b;
	if (a<b) {
		int t = a;
		a = b;
		b = t;
	}
	while (b) {
		int a1 = a%b;
		a = b;
		b = a1;
	}
	return a;
}

double iexp(double e, unsigned n) {
	double f = 1;
	double d = e;
	while (n) {
		if (n&1)
			f *= d;
		d = d*d;
		n >>= 1;
	}
	return f;
}

double ilogexp(double e, unsigned n) {
	double f = 0;
	double d = log(e);
	while (n) {
		if (n&1)
			f += d;
		d *= 2;
		n >>= 1;
	}
	return f;
}


int d183 (int n) {
	int sign;
	double max = -1;
	int mk = -1;
	for(int k=1;k<=n;++k) {
		double v = ilogexp(n/double(k),k);
		if (v>max) {
			mk = k;
			max = v;
			sign = only25(k/gcd(n,k))?-1:1;
		}
	}
//	cout << "d183 " << n << " " << max << " " << mk << endl;
	return sign*n;
}

long long p183(int n) {
	long long s = 0;
	for(int i=5;i<=n;i++)
		s += d183(i);
	return s;
}

int p51() {
	vector<int> ps;
	sieve(10000000,ps);
//	cout << ps.size() << endl;

	int n = ps.size();
	for(int i=0;i<n;i++) {
		int p = ps[i];
		if (p<100000)
			continue;
		int r = the6Form8(p);
		if (r!=-1)
			return r;
	}
	return -1;
}

long long p100 (long long m) {
	long long sm = m*(m-1)/2;
	long long n = sqrt(sm)-1;
	long long sn = n*(n-1);
	while (1) {
		while (sn < sm) {
			sn += n+n;
			n++;
		}
		cout << n << endl;
		if (sn==sm)
			return n;
		while (sn > sm) {
			sm += m;
			m++;
		}
	}
}

class BigIntRadix {
	vector<unsigned char> a; // little endian
	unsigned radix;
	char i2c(const unsigned char i) const {
		if (i < 10)
			return '0' + i;
		if (i < 36)
			return 'A' + (i - 10);
		cerr << i << endl;
		throw "i2c";
	}
	unsigned char c2i(const char c) const {
		if (c >= '0' && c <= '9')
			return c - '0';
		if (c >= 'A' && c <= 'Z')
			return c - 'A' + 10;
		cerr << c << endl;
		throw "c2i";
	}
public:
	BigIntRadix(const vector<unsigned char>& _a, unsigned _radix) :
		a(_a), radix(_radix) {
	}
	BigIntRadix(const string& str, unsigned radix);
	void print() const; // print as decimal
	unsigned div(int divisor); // returns the residue
	bool isZero() const; // can keep a flag to make it O(1)
	void convert(int newRadix);
};

BigIntRadix::BigIntRadix(const string& str, unsigned _radix) {
	vector<unsigned char> v;
	int n = str.length();
	for(int i=n-1;i>=0;i--) {
		v.push_back(c2i(str[i]));
	}
	a = v;
	radix = _radix;
}

void BigIntRadix::convert(int newRadix) {
	vector<unsigned char> b;
	while (!isZero()) {
		b.push_back(div(newRadix));
	}
	a = b;
	radix = newRadix;
}

bool BigIntRadix::isZero() const {
	for (vector<unsigned char>::const_iterator it = a.begin(); it != a.end(); ++it) {
		if (*it != 0)
			return false;
	}
	return true;
}

unsigned BigIntRadix::div(int n) {
	if (n <= 0)
		throw "BigInt::div n <=0";
	unsigned alen = a.size();
	unsigned r = 0;
	for (int i = alen - 1; i >= 0; --i) {
		unsigned t = (radix * r + a[i]) / n;
		r = (radix * r + a[i]) - t * n;
		a[i] = t;
	}
	return r;
}

// TODO: overall O(n^2), may re-use some intermediate results?
// print as 10-radix, little endian
void BigIntRadix::print() const {
	int n = a.size();
	bool leading = true;
	for (int i = n - 1; i >= 0; i--) {
		if (leading && !a[i])
			continue;
		leading = false;
		cout << i2c(a[i]);
	}
	cout << endl;
}

bool isPlaindrom(int m) {
	vector<int> v;
	int2rv(m,10,v);
	int n = v.size();
	for (int i=0;i<=n/2;i++) {
		if (v[i]!=v[n-1-i]) return false;
	}
	return true;
}

void TCONNUM() {
	int n;
	cin >> n;
	string str2;
	getline(cin,str2);
//	cout << n << endl;
	for(int i=0;i<n;i++) {
		string ln;
		getline(cin,ln);
//		cout << ln << endl;
		istringstream ins(ln);
		string str;
		int r;
		int s;
		ins >> str >> r >> s;
//		cout << str << " " << r << " " << s <<endl;
		BigIntRadix bi(str,r);
		bi.convert(s);
		bi.print();
	}
}

void filterPP(const vector<int>& ps, vector<int>& pps) {
	int n = ps.size();
	for(int i=0;i<n;i++) {
		int p = ps[i];
		if (isPlaindrom(p))
			pps.push_back(p);
	}
}

int digitProdcut(int pp) {
	vector<int> v;
	int2rv(pp,10,v);
	int p = 1;
	for(vector<int>::iterator it = v.begin();it!=v.end();++it) {
		int n = *it;
		if (n) p*=n;
	}
	return p;
}

void MB1() {
	int n;
	cin >> n;
	vector<int> ps;
	sieve(1000000,ps);
	vector<int> pps;
	filterPP(ps,pps);
	{
		string str2;
		getline(cin,str2);
	}
	for(int i=0;i<n;i++) {
		int n;
		cin >> n;
		int pp = pps[n-1];
		cout << pp << "," <<  ps[digitProdcut(pp)-1] << ",";
		string str2;
		getline(cin,str2);
	}
	cout << endl;
}

void TDKPRIME() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	vector<int> ps;
	sieve_odd(100000000,ps);
//	sieve_odd_bs(100000000,ps);
	for(int i=0;i<n;i++) {
		int n;
		cin >> n;
		if(n==1) cout << 2 << endl;
		else cout << ps[n-1] << endl;
		string str2;
		getline(cin,str2);
	}
}

long long eval(const vector<int>& cs, int k) {
//	cout << "eval " << k << endl;
	long long s = cs[0];
	int n = cs.size();
	for(int i=1;i<n;i++) {
		s = s*k+cs[i];
	}
	return s;
}

int POLEVAL() {
	int n;
	int cas = 1;
	while (1) {
		cin >> n;
//		cout << "n = " << n << endl;
		{
			string str2;
			getline(cin,str2);
		}
		if (n==-1) break;
		vector<int> cs;
		string str;
		for(int i=0;i<n;i++) {
			getline(cin,str,' ');
//			cout << str << endl;
			cs.push_back(atoi(str.c_str()));
		}
		getline(cin,str);
		cs.push_back(atoi(str.c_str()));
//		{
//			string str2;
//			getline(cin,str2);
//		}
		cin >> n;
//		cout << "k = " << n << endl;
		{
			string str2;
			getline(cin,str2);
		}
		cout << "Case "<< cas << ":" << endl;
		cas ++;
		for(int i=0;i<n-1;i++) {
			getline(cin,str,' ');
			int k = atoi(str.c_str());
			cout << eval(cs,k) << endl;
		}
		getline(cin,str);
		int k = atoi(str.c_str());
		cout << eval(cs,k) << endl;
//		{
//			string str2;
//			getline(cin,str2);
//		}

	}
	return 0;
}

int TOANDFRO() {
	while (1) {
		int n;
		cin >> n;
		{
			string str2;
			getline(cin,str2);
		}
		if (!n) break;
		string ln;
		getline(cin,ln);
		string ln2(ln);
		int m = ln.length();
		int k = m/n;
//		cout << k << endl;
		for(int i=0;i<m;i++) {
			if (((i%k)&1)==0) {
				ln2[i] = ln[(i%k)*n+i/k];
			} else {
				ln2[i] = ln[(i%k)*n+n-1-i/k];
//				ln2[i] = ' ';
			}
		}
		cout << ln2 << endl;
	}
	return 0;
}

void testXY(int x,int y) {
	if (x<0 || y<0 || (x!=y && x!=y+2)) {
		cout << "No Number" << endl;
	} else {
		if (x==y) {
			if (x&1) {
				cout << 4*(x/2) +1 << endl;
			} else
				cout << 4*(x/2) << endl;
		} else {
			if (x&1) {
				cout << 2+4*((x-2)/2)+1 << endl;
			} else {
				cout << 2+4*((x-2)/2) << endl;
			}
		}
	}

}

int NSTEPS() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	for(int i=0;i<n;i++) {
		int x,y;
		cin >> x >> y;
		testXY(x,y);
		{
			string str2;
			getline(cin,str2);
		}
	}
	return 0;
}

void doFAMILYP(int k) {
	int n = floor(sqrt(2*k)) -2;
	while (1) {
		int n2 = n*(n+1);
		if (n2 > 2*k){
			n--;
			break;
		}
		if (n2 == 2*k){
			cout << "TERM " << k << " IS " << char('A'+(n-1)%26) << endl;
			return;
		}
		n++;
	}
	cout << "TERM " << k << " IS " << char('A'+(n)%26) << endl;
}

int FAMILYP() {
	int t;
	while(cin >> t) {
		doFAMILYP(t);
	}
	return 0;
}

int AMR10F(){
	int t;
	cin >> t;
	while(t--) {
		int n,a,d;
		cin >> n>> a>> d;
		cout << n*a+n*(n-1)/2*d << endl;
	}
	return 0;
}

void doQUALITY(int&s, int& m,int a,int b){
	if(a) {
		s+=1;
		m+=a+(b-1)*1200;
	}
}

int QUALITY() {
	int i=0;
	int a[3],b[3];
	while(cin >> a[0]>>a[1]>>a[2]>>b[0]>>b[1]>>b[2]) {
		int s=0,m=0;
		for(int j=0;j<3;j++)
			doQUALITY(s,m,a[j],b[j]);
		cout << "team " << i+1<<": "<< s<< ", "<< m<< endl;
		i++;
	}
	return 0;
}

long long int calc(long long int s,char op, long long int s2) {
	switch(op) {
	case '+':return s+s2;
	case '-':return s-s2;
	case '*':return s*s2;
	case '/':return s/s2;
	}
	return 0;
}

int ARITH2(){
	string ln;
	getline(cin,ln);
	int t = atoi(ln.c_str());
	while(t--) {
		getline(cin,ln);
		getline(cin,ln);
		int n = ln.size();
		vector<int> v;
		char op=-1;
		long long int s;
		for(int i=0;i<n;i++) {
			char c = ln[i];
			switch(c) {
			case '+':
			case '-':
			case '*':
			case '/':
				op = c;
			case '=':
			case ' ':
				if(!v.empty()) {
					reverse(v.begin(),v.end());
					long long int a = rv2int(v,10);
//					cout << a<< endl;
					if(op!=-1)
						s = calc(s,op,a);
					else
						s = a;
					v.clear();
				}
				break;
			default:
				if(c>='0' && c<='9')
					v.push_back(c-'0');
				break;
			}
		}
		cout << s<< endl;
	}
	return 0;
}

int auxToIntNSYSTEM(const string& str,char c) {
	unsigned m = str.find(c);
	if (m!=string::npos) {
		if (m==0)
			return 1;
		else {
			char c = str[m-1];
			if(c>='2' && c<='9')
				return (c-'0');
			else
				return 1;
		}

	}
	return 0;
}
int toIntNSYSTEM(const string& str) {
//	cout << str << endl;
	return auxToIntNSYSTEM(str,'m')*1000+auxToIntNSYSTEM(str,'c')*100+
			auxToIntNSYSTEM(str,'x')*10+auxToIntNSYSTEM(str,'i');
}
void auxOfIntNSYSTEM(int& n,string& str,char c,int m) {
	int i = n/m;
	if(i) {
		if(i!=1)
			str.append(1,char(i+'0'));
		str.append(1,c);
	}
	n -= i*m;
}
void ofIntNSYSTEM(int n,string& str) {
	str = "";
	auxOfIntNSYSTEM(n,str,'m',1000);
	auxOfIntNSYSTEM(n,str,'c',100);
	auxOfIntNSYSTEM(n,str,'x',10);
	auxOfIntNSYSTEM(n,str,'i',1);
}

int NSYSTEM() {
	int n;
	cin>> n;
	{
		string str2;
		getline(cin,str2);
	}
	while(n--){
		string ln;
		getline(cin,ln);
//		cout << ln << endl;
		int ws = ln.find_first_of(" \t");
//		cout << ws << endl;
		int i = toIntNSYSTEM(ln.substr(0,ws))+toIntNSYSTEM(ln.substr(ws,ln.length()-ws));
		ofIntNSYSTEM(i,ln);
		cout << ln << endl;
	}
	return 0;
}

void doCANTON(int k) {
	int n = floor(sqrt(2*k)) -2;
	while (1) {
		int n2 = n*(n+1);
		if (n2 > 2*k){
			n--;
			break;
		}
		if (n2 == 2*k){
			if (n&1) {
				cout << "TERM " << k << " IS " << 1 << "/" << n << endl;
			} else {
				cout << "TERM " << k << " IS " << n << "/" << 1 << endl;
			}
			return;
		}
		n++;
	}
	int n2 = n+1;
	int d = k - n*(n+1)/2 -1;
	if (n2&1) {
		cout << "TERM " << k << " IS " << n2-d << "/" << 1+d << endl;
	} else {
		cout << "TERM " << k << " IS " << 1+d << "/" << n2-d << endl;
	}
}

int CANTON() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	for(int i=0;i<n;i++) {
		int t;
		cin >> t ;
		doCANTON(t);
		{
			string str2;
			getline(cin,str2);
		}
	}
	return 0;
}

int doSAMER08F(int n) {
	int s = 0;
	for(int i=1;i<=n;i++) {
		s += (n-i+1)*(n-i+1);
	}
	return s;
}

int SAMER08F() {

	while(1) {
		int t;
		cin >> t ;
		if (!t) break;
		cout << doSAMER08F(t) << ",";
		{
			string str2;
			getline(cin,str2);
		}
	}
	return 0;
}

int GNY07A() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	for(int i=0;i<n;i++) {
		int t;
		cin >> t ;
		string str;
		cin >> str;
		str.erase(t-1,1);
		cout << i+1 << " " << str << endl;
		{
			string str2;
			getline(cin,str2);
		}
	}
	return 0;
}

int CANDY(){
	while(1) {
		int t;
		cin >> t ;
		if (t==-1) break;
		{
			string str2;
			getline(cin,str2);
		}
		vector<int> v;
		int s = 0;
		for(int i=0;i<t;i++) {
			int n;
			cin >> n;
			{
				string str2;
				getline(cin,str2);
			}
			s += n;
			v.push_back(n);
		}
		if (s%t)
			cout << -1 << endl;
		else {
			int ss = 0;
			int a  =s/t;
			for(int i=0;i<t;i++) {
				if (v[i]>a)
					ss += v[i]-a;
			}
			cout << ss << endl;
		}
	}
	return 0;
}

bool doPERMUT2(vector<int>& v) {
	int n = v.size();
	vector<int> v2(n);
	for(int i=0;i<n;i++) {
		v2[v[i]-1] = i+1;
		if (v2[i] && v[i] && v2[i]!=v[i])
			return false;
	}
	for(int i=0;i<n;i++) {
		if (v2[i]!=v[i]) return false;
	}
	return true;
}

int PERMUT2() {
	while(1) {
		int t;
		cin >> t ;
//		cout << "t= " << t << endl;
		if (!t) break;

		{
			string str2;
			getline(cin,str2);
		}
		vector<int> v;
		for(int i=0;i<t-1;i++) {
			string str;
			getline(cin,str,' ');
			v.push_back(atoi(str.c_str()));
		}
		{
			string str2;
			getline(cin,str2);
			v.push_back(atoi(str2.c_str()));
		}
//		cout << v.size() << endl;
		if (doPERMUT2(v))
			cout << "ambiguous" << endl;
		else
			cout << "not ambiguous" << endl;
	}
	return 0;
}

int QUADAREA() {
	int n;
	cin >> n;
	cout.setf(ios::fixed, ios::floatfield);
	cout.precision(2);
	while(n--) {
		double a,b,c,d;
		cin >> a>> b>> c>> d;
		double p = (a+b+c+d)/2.;
		cout << sqrt((p-a)*(p-b)*(p-c)*(p-d)) << endl;
	}
	return 0;
}

int WORDCNT() {
	int n;
	cin >> n;
	string str2;
	getline(cin,str2);
	while(n--) {
		string ln;
		getline(cin,ln);
		stringstream ss(ln);
		string t;
		int cnt;
		int mcnt = -1;
		int ll = -1;
		while(ss>>t) {
//			cout << t << endl;
			int l = t.length();
			if(l==ll) {
				cnt++;
			} else {
				if (cnt >mcnt)
					mcnt = cnt;
				ll = l;
				cnt = 1;
			}
		}
		cout << mcnt << endl;
	}
	return 0;
}

int BYTESM2() {
	int t;
	cin >> t;
	while(t--) {
		int h,w;
		cin >> h >> w;
//		cout << h << " " << w << endl;
		vector<int> vs[2];
		vs[0].reserve(w);
		vs[1].reserve(w);
		for(int i=0;i<w;i++) {
			vs[0][i] = vs[1][i] =0;
		}
		int mmax = 0;
		while(h--) {
			for(int i=0;i<w;i++) {
				int m = 0;
				if(i)
					m = max(m,vs[h&1][i-1]);
				if(i<w-1)
					m = max(m,vs[h&1][i+1]);
				m = max(m,vs[h&1][i]);
				int k;
				cin >> k;
				m += k;
				mmax = max(mmax,m);
				vs[(h+1)&1][i] = m;
			}
//			cout << mmax << endl;
		}
		cout << mmax << endl;
	}
	return 0;
}

int SUMITR() {
	int t;
	cin >> t;
	while(t--) {
		int w;
		cin >> w;
		vector<int> vs[2];
		vs[0].reserve(w);
		vs[1].reserve(w);
		for(int i=0;i<w;i++)
			vs[0][i] = vs[1][i] =0;
		int mmax = 0;
		for(int j=0;j<w;j++)
			for(int i=0;i<=j;i++) {
				int m = vs[j&1][i];
				if(i)
					m = max(m,vs[j&1][i-1]);
				int k;
				cin >> k;
				m += k;
				mmax = max(mmax,m);
				vs[(j+1)&1][i] = m;
			}
		cout << mmax << endl;
	}
}

int p78(int n,int k=1){
	if (k>n) return 0;
	if (2*k>n) return 1;
	static map<pair<int,int>, int >m;
	pair<int,int> nk = make_pair(n,k);
	if(m.find(nk)!=m.end())
		return m[nk];
	int s = 1;
	for(int i=k;i<=n/2;i++)
		s += p78(n-i,i);
	m[nk] = s;
	return s;
}

void p78_() {
	int i = 1;
	while(p78(i)%1000000)
		i++;
	cout << i << endl;
}

bool isSquare(int n){
	int n2= sqrt(n);
	return (n2*n2)==n;
}

int t_p86(int i,int j,int k) {
	return isSquare((i+j)*(i+j)+k*k);
}

int f_p86(int m) {
	int s = t_p86(m,m,m);
	for(int i=1;i<m;i++)
		s+=t_p86(i,m,m);
	for(int i=1;i<m;i++)
		for(int j=1;j<=i;j++)
			s+=t_p86(j,i,m);
	return s;
}

int g_p86(int m){
	int s=0;
	for(int i=1;i<=m;i++)
		s+=f_p86(i);
	return s;
}

int MISERMAN() {
//	int t;
//	cin >> t;
	{
		int h,w;
		cin >> h >> w;
//		cout << h << " " << w << endl;
		vector<int> vs[2];
		vs[0].reserve(w);
		vs[1].reserve(w);
		for(int i=0;i<w;i++) {
			vs[0][i] = vs[1][i] =0;
		}
		int mmax;
		while(h--) {
			mmax = 1000;
			for(int i=0;i<w;i++) {
				int m = 1000;
				if(i)
					m = min(m,vs[h&1][i-1]);
				if(i<w-1)
					m = min(m,vs[h&1][i+1]);
				m = min(m,vs[h&1][i]);
				int k;
				cin >> k;
				m += k;
				mmax = min(mmax,m);
				vs[(h+1)&1][i] = m;
			}
//			cout << mmax << endl;
		}
		cout << mmax << endl;
	}
	return 0;
}

void doHANGOVER(double *a, double d) {
	int i1 = int (exp(d - (0.5772156 - 1)));
	for(int i=-1;i<2;i++)
		if (a[i+i1]>=d) {
			cout << i+i1 << " card(s)" << endl;
			return;
		}
}

int HANGOVER() {
	double a[300];
	double s =0;
	for(int i=1;i<300;i++) {
		s += 1./(i+1);
		a[i] = s;
	}

	double eps = 0.01;
	while(1) {
		double d;
		cin >> d;
		if (d<eps)
			break;
		{
			string str2;
			getline(cin,str2);
		}
		doHANGOVER(a,d);
	}
	return 0;
}

unsigned doCOINS(unsigned n) {
	static map<unsigned,unsigned> m;
	if (n==0 || n==1) return n;
	if (m.find(n)!=m.end())
		return m[n];
	unsigned a = max(doCOINS(n/2) + doCOINS(n/3) + doCOINS(n/4),n);
	m[n] = a;
//	cout << n << " " << a << endl;
	return a;
}

int COINS () {
	while(1){
		int n;
		if(!(cin >> n)) break;
		cout << doCOINS(n) << endl;
		{
			string str2;
			if(!getline(cin,str2)) break;
		}
	}
	return 0;
}

int exp10(int a, int b) {
	int p = 1;
	int t = a;
	while(b) {
		if(b&1)
			p=(p*t)%10;
		t=t*t;
		b>>=1;
	}
	return p;
}

int LASTDIG() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	for(int i=0;i<n;i++) {
		int a,b;
		string str;
		getline(cin,str,' ');
		a = atoi(str.c_str());
		cin >> b;
//		cout << a << b << endl;
		cout << exp10(a,b) << endl;
		{
			string str2;
			getline(cin,str2);
		}
	}
	return 0;
}

long long mod_long(long long a,long long b) {
	return a-a/b*b;
}

int CANDY3() {
	int n;
	cin >> n;
	{
		string str2;
		getline(cin,str2);
	}
	int cnt = 0;
	bool realFirst = true;
	bool first = true;
	long long s = 0;
	long long m = 0;
	while(1) {
		string str;
		if(!getline(cin,str) || str=="") {
			if (!realFirst) {
				if(s==0)
					cout << "YES" << endl;
				else
					cout << "NO" << endl;
				s = 0;
				first = true;
			} else
				realFirst = false;
			cnt++;
			if(cnt==n+1) break;
		} else {
			if (first) {
				m = atoll(str.c_str());
				first = false;
			} else {
				s = mod_long(s +atoll(str.c_str()),m);
			}
		}
	}
	return 0;
}

long long doACODE(const string& s,int i,int n,map<long long,int>& m) {
	if(i==n)return 1;
	if(i>n)return 0;
	if(m.find(i)!=m.end())
		return m[i];
	long long r;
	switch(s[i]) {
	case '0':
		r = 0;
		break;
	case '1':
		r = doACODE(s,i+1,n,m) + doACODE(s,i+2,n,m);
		break;
	case '2':
		if(i==n-1)
			r = 1;
		else {
			char c = s[i+1];
			if(c<='6')
				r = doACODE(s,i+1,n,m) + doACODE(s,i+2,n,m);
			else
				r = doACODE(s,i+1,n,m);
		}
		break;
	default:
		r = doACODE(s,i+1,n,m);
		break;
	}
	m[i]  = r;
	return r;
}

int ACODE() {
	while(1){
		string ln;
		getline(cin,ln);
		if(ln=="" || ln[0]=='0') break;
		map<long long,int> m;
		cout << doACODE(ln,0,ln.length(),m) << endl;
	}
	return 0;
}

bool doSTPAR(vector<int>& v) {
	int n = v.size();
	vector<int> s;
	int cnt = 1;
	for(int i=0;i<n;) {
		int t = v[i];
//		cout << t << endl;
		if(t==cnt) {
			cnt++;
			i++;
		} else {
			if (!s.empty() && cnt==s.back()) {
				cnt ++;
				s.pop_back();
			}
			else {
				s.push_back(t);
				i++;
			}
		}
	}
	while(!s.empty()) {
		if(cnt!=s.back())
			return false;
		cnt ++;
		s.pop_back();
	}
	return true;
}

int STPAR() {
	while(1) {
		int n;
		cin >> n;
		{
			string str2;
			getline(cin,str2);
		}
		if (!n) break;
		vector<int> v;
		string str;
		for(int i=0;i<n-1;i++) {
			getline(cin,str,' ');
			v.push_back(atoi(str.c_str()));
		}
		getline(cin,str);
		v.push_back(atoi(str.c_str()));
		if (doSTPAR(v))
			cout << "yes" << endl;
		else
			cout << "no" << endl;
	}
	return 0;
}

void doABSYS(string& s) {
	int plus = s.find('+');
	string s1 = s.substr(s.find_first_not_of(" \t"),s.find_first_of(" \t"));
	int p2 = s.find_first_not_of(" \t",plus+1);
	string s2 = s.substr(p2,s.find_first_of(" \t",p2+1)-p2);
	int eq = s.find('=');
	int p3 = s.find_first_not_of(" \t",eq+1);
	string s3 = s.substr(p3,s.find_first_of(" \t",p3+1)-p3);
//	cout << s1 << "#" << s2 << "#" << s3 << endl;
	int n1,n2,n3;
	if (s1.find('m')!=string::npos) {
		n3 = atoi(s3.c_str());
		n2 = atoi(s2.c_str());
		n1 = n3 -n2;
	}
	if (s2.find('m')!=string::npos) {
		n3 = atoi(s3.c_str());
		n1 = atoi(s1.c_str());
		n2 = n3 -n1;
	}
	if (s3.find('m')!=string::npos) {
		n1 = atoi(s1.c_str());
		n2 = atoi(s2.c_str());
		n3 = n1 + n2;
	}
	cout << n1 << " + " << n2 << " = " << n3 << endl;
}

int ABSYS() {
	string str;
	getline(cin,str);
	int n = atoi(str.c_str());
	for(int i=0;i<n;i++) {
		getline(cin,str);
		getline(cin,str);
		doABSYS(str);
	}
	return 0;
}

void factors(int n,vector<int>& fs) {
	if (n<0) n = -n;
	fs.clear();
	if (!n) return;
	for(int i=1;i<=n;i++) {
		if(n%i==0)
			fs.push_back(i);
	}
}

void smallFactors(int n,vector<int>& fs) {
	if (n<0) n = -n;
	fs.clear();
	if (!n) return;
	for(int i=1;i*i<=n;i++) {
		if(n%i==0)
			fs.push_back(i);
	}
}

void doAE00(int n){
	int s = 0;
	for(int i=1;i<=n;i++) {
		vector<int> v;
		smallFactors(i,v);
		s += v.size();
	}
	cout << s << endl;
}

int AE00() {
	int n;
	cin >> n;
	doAE00(n);
	return 0;
}

//bool testConflictBISHOP(const vector<int>& v,int i,int j) {
//	int k = i-j;
//	i--;
//	while(i>=0) {
//		if(i-v[i]==k)
//			return true;
//	}
//	return false;
//}

void exhaustiveBISHOP(int n,int k, vector<int>& v) {
	if(k==2*n-1) {
		int m = v.size();
		for(int i =0;i<m;i++)
			cout << v[i] << " ";
		cout << endl;
		return;
	}
	int i0 = k>=n?k-n+1:0;
	int j0 = k>=n?n-1:k;
	int lim = k>=n?2*n-1-k:1+k;
	for(int i=0;i<lim;i++) {
		int ii = i0+i;
		int jj = j0-i;
//		if (testConflictBISHOP(v,ii,jj)) continue;
//		v[ii]
	}
}

int MIRRORED() {
	cout << "Ready" << endl;
	while(1) {
		string str;
		if (!getline(cin,str)||str=="  ") break;
		if(str=="pq" || str == "qp" || str == "bd" || str == "db")
			cout << "Mirrored pair" << endl;
		else
			cout << "Ordinary pair" << endl;
	}
	return 0;
}

int GNY07B() {
	string str;
	getline(cin,str);
	int n = atoi(str.c_str());
	for(int i=0;i<n;i++) {
		double f;
		getline(cin,str,' ');
		f = atof(str.c_str());
		getline(cin,str);
		cout << i+1 << " ";
		cout.setf(ios::fixed, ios::floatfield);
		cout.precision(4);
		if (str== "kg")
			cout << f*2.2046 << " "  << "lb" << endl;
		if (str=="l")
			cout << f*0.2642 << " "  << "g" << endl;
		if (str== "lb")
			cout << f*0.4536 << " "  << "kg" << endl;
		if (str=="g")
			cout << f*3.7854 << " "  << "l" << endl;
	}
	return 0;
}

int maxLine(int n) {
	string str;
	int m = 0;
	for(int i=0;i<n-1;i++) {
		getline(cin,str,' ');
		m = max(m,atoi(str.c_str()));
	}
	getline(cin,str);
	m = max(m,atoi(str.c_str()));
	return m;
}

int ARMY(){
	string str;
	getline(cin,str);
	int t = atoi(str.c_str());
	for(int i=0;i<t;i++) {
		getline(cin,str);
		int ng,nm;
		getline(cin,str,' ');
		ng = atoi(str.c_str());
		getline(cin,str);
		nm = atoi(str.c_str());
		int a = maxLine(ng);
		int b = maxLine(nm);
		if (a>=b)
			cout << "Godzilla" << endl;
		else
			cout << "MechaGodzilla" << endl;
	}
	return 0;
}

void readLine(int n,vector<int>& v,char sep=' ') {
	v.clear();
	string str;
	for(int i=0;i<n-1;i++) {
		getline(cin,str,sep);
		v.push_back(atoi(str.c_str()));
	}
	getline(cin,str);
	v.push_back(atoi(str.c_str()));
}

int AE1B() {
	string str;
	int n,k,s;
	getline(cin,str,' ');
	n = atoi(str.c_str());
	getline(cin,str,' ');
	k = atoi(str.c_str());
	getline(cin,str);
	s = atoi(str.c_str());
	vector<int> v;
	readLine(n,v);
	sort(v.begin(),v.end());
	int sk = s*k;
	int sum = 0;
	int i;
	for(i=1;;i++) {
		sum += v[n-i];
		if(sum>=sk)
			break;
	}
	cout << i << endl;
	return 0;
}

int STAMPS() {
	int n;
	cin>>n;
	for(int j=0;j<n;j++) {
		int s,k;
		cout << "Scenario #" << j+1 << ":" << endl;
		cin >> s>> k;
		vector<int> v(k);
		for(int i=0;i<k;i++)
			cin>>v[i];
		sort(v.begin(),v.end());
		int ss =0;
		int i;
		for(i=0;i<k;i++){
			ss+=v[k-1-i];
			if(ss>=s) {
				cout << i+1 << endl << endl;
				break;
			}
		}
		if (i==k)
			cout << "impossible" << endl << endl;
	}
	return 0;
}

int tryPIGBANK(int s, int k,const vector<pair<int,int> >& vw, map<pair<int,int>,int >& m) {
	if(s==0)
		return 0;
	if (k==-1)
		return INT_MAX;
//	pair<int,int> sk = make_pair(s,k);
//	if(m.find(sk)!=m.end()) {
//		return m[sk];
//	}
	pair<int,int> p = vw[k];
	int v = p.first;
	int w = p.second;
	int mmin = INT_MAX;
	for(int i=s/w;i>=0;i--) {
		int r = tryPIGBANK(s-i*w,k-1,vw,m);
		if (r!=INT_MAX)
			return i*v+r;
//			mmin = min(mmin, i*v+r);
	}
	return INT_MAX;
//	m[sk] = mmin;
//	return mmin;
}

bool by_density(const pair<int,int>& a, const pair<int,int>& b) {
        return a.first*b.second > a.second*b.first;
}

void doPIGBANK2(int s,vector<pair<int,int> >& vw) {
	int n = vw.size();
	sort(vw.begin(),vw.end(),by_density);
//	for(int i=0;i<n;i++)
//		cout << vw[i].first << " " << vw[i].second <<  endl;
	map<pair<int,int>,int > m;
	int r = tryPIGBANK(s,n-1,vw,m);
	if(r==INT_MAX)
		cout << "This is impossible." << endl;
	else
		cout << "The minimum amount of money in the piggy-bank is " << r << "." << endl;
}

void doPIGBANK(int s,vector<pair<int,int> >& vw) {
	const int large = 1000000000;
	int m = s+1;
	vector<int> a(m);
	for(int i=0;i<m;i++)
		a[i] = large;
	a[0] = 0;
	int n = vw.size();
	for(int i=0;i<n;i++) {
		pair<int,int> p = vw[i];
		int v = p.first;
		int w = p.second;
		for(int i=1;i<=s/w;i++) {
			int w0 = w*i;
			int v0 = a[w0-w] + v;
			if(a[w0] > v0)
				a[w0] = v0;
		}
	}
	if (a[s]==large)
		cout << "This is impossible." << endl;
	else
		cout << "The minimum amount of money in the piggy-bank is " << a[s] << "." << endl;
}

int PIGBANK() {
	string str;
	getline(cin,str);
	int n = atoi(str.c_str());
	for(int i=0;i<n;i++) {
		getline(cin,str,' ');
		int e = atoi(str.c_str());
		getline(cin,str);
		int f = atoi(str.c_str());
		getline(cin,str);
		int m = atoi(str.c_str());
		vector<pair<int,int> > vw;
		for(int j=0;j<m;j++) {
			getline(cin,str,' ');
			int v = atoi(str.c_str());
			getline(cin,str);
			int w = atoi(str.c_str());
			vw.push_back(make_pair(v,w));
		}
		doPIGBANK(f-e,vw);
	}
	return 0;
}

void tovecNEG2(int n,vector<int>& v, bool flip=false) {
	int c = 0;
	int i =0;
	while (n || c) {
		if ((i&1)xor flip) {
			if (n&1) {
				if(c&1) {
					v.push_back(0);
					c >>=1;
				} else {
					v.push_back(1);
					c = (c>>1)+1;
				}
			} else {
				v.push_back(c&1);
				c >>=1;
			}
		} else {
			if (n&1) {
				if (c&1) {
					v.push_back(0);
					c=(c>>1) + 3;
				} else {
					v.push_back(1);
					c>>=1;
				}
			} else {
				v.push_back(c&1);
				c >>=1;
			}
		}
		i++;
		n=n/2;
	}
	reverse(v.begin(),v.end());
}

int NEG2(int n) {
	if (!n) {
		cout << "0" << endl;
		return 0;
	}
	vector<int> v;
	if (n>0)
		tovecNEG2(n,v);
	else
		tovecNEG2(-n,v,true);
	int nn = v.size();
	for(int i=0;i<nn;i++)
		cout << v[i];
	cout << endl;
	return 0;
}

void doEASYPROB(int k){
	if(k==0) {
		cout << "0";
		return;
	} else if(k==2) {
		cout << "2";
		return;
	}
	vector<int> v;
	int2rv(k,2,v);
	reverse(v.begin(),v.end());
	int n = v.size();
	int maxi = -1;
	for(int i=0;i<n;i++) {
		if(v[i])
			maxi = i;
	}
	for(int i=0;i<n;i++) {
		if(v[i]) {
			if (n-1-i==1)
				cout << "2";
			else {
				cout <<"2(";
				doEASYPROB(n-1-i);
				cout << ")";
			}
			if(i!=maxi)
				cout << "+";
		}
	}
}

int EASYPROB(){
	int a[] = {137, 1315, 73, 136, 255, 1384, 16385};
	for(int i=0;i<7;i++) {
		cout <<a[i]<<"=";
		doEASYPROB(a[i]);
		cout << endl;
	}
	return 0;
}

int HELLOKIT() {
	while(1) {
		string w;
		getline(cin,w,' ');
		if (w[0]=='.') break;
//		cout << "w= "<< w << endl;
		int n;
		cin >> n;
		{
			string str;
			getline(cin,str);
		}
//		cout << n << endl;
		int m = w.length();
		for(int i=0;i<m;i++) {
			cout << w.substr(i,m-i);
			for(int j=1;j<n;j++)
				cout << w;
			cout << w.substr(0,i);
			cout << endl;
		}
	}
	return 0;
}

int OFFSIDE() {
	while(1) {
		int a,d;
		cin >> a>> d;
		if(!a) break;
		int ma = INT_MAX;
		int p;
		while(a--) {
			cin >> p;
			ma = min(ma,p);
		}
		int mb = INT_MAX,mb2 = INT_MAX;
		while(d--) {
			cin >> p;
			if (p<mb2) {
				if (p<mb) {
					mb = p;
					mb2 = mb;
				}
				else
					mb2 = p;
			}
		}
//		cout << ma << " " << mb << " " << mb2 << endl;
		if (ma<mb2)
			cout << "Y" << endl;
		else
			cout << "N" << endl;
	}
	return 0;
}

int FENCE1(){
	int l;
	cout.setf(ios::fixed, ios::floatfield);
	cout.precision(2);
	double pi = 3.14159265358;
	while(1) {
		cin >> l;
		if(!l) break;
		cout << l*l/pi/2 << endl;
	}
	return 0;
}

int ANARC08E(){
	int a,b;
	while(1) {
		cin >> a>> b;
		if(a==-1) break;
		cout <<a<<"+"<<b;
		if(a==1||b==1)
			cout << "=";
		else
			cout << "!=";
		cout << a+b<< endl;
	}
	return 0;
}

int CPRMT(){
	string a,b;
	while(1) {
		if(!getline(cin,a))break;
		if(!getline(cin,b))break;
		sort(a.begin(),a.end());
		sort(b.begin(),b.end());
		int n=a.length();
		int n2=b.length();
		int j=0;
		for(int i=0;i<n && j<n2;) {
			if(a[i]==b[j]) {
				cout << a[i];
				i++;
				j++;
			} else if (a[i]<b[j]) {
				i++;
			} else {
				j++;

			}
		}
		cout << endl;
	}
	return 0;
}

int ACPC10A(){
	int a,b,c;
	while(1) {
		cin >> a>> b>> c;
		if(!a&&!b&&!c)break;
		if(2*b==a+c)
			cout << "AP " << 2*c-b << endl;
		else
			cout << "GP " << c*c/b << endl;
	}
	return 0;
}

int ALCHE(){
	while(1) {
		int a,b;
		cin >> a>> b;
		if(a==-1 && b==-1) break;
		if (a*37==b*1000)
			cout << "Y" << endl;
		else
			cout << "N" << endl;
	}
	return 0;
}

int GIRLSNBS() {
	while(1) {
		int g,b;
		cin >> g>> b;
		if(g==-1&&b==-1) break;
		if (g>b){
			int t = g;
			g = b;
			b = t;
		}
		cout << ceil(b/(g+1.)) << endl;
	}
	return 0;
}

int TRGRID() {
	int n;
	cin >> n;
	char d[] = {'U','R','D','L'};
	while(n--) {
		int n,m;
		cin >> n>>m;
		if(n>m)
			cout << d[(2*(m-1)+2)%4] << endl;
		else
			cout << d[(2*(n-1)+1)%4] << endl;
	}
	return 0;
}

int doEXFOR(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9, int x10) {
	return (x1 || x2)^(x1 || x3)^(x1 || x4)^(x1 || x5)^(x1 || x6)^(x1 || x7)^(x1 || x8)^(x1 || x9)^(x1 || x10)^(x2 || x3)^(x2 || x4)^(x2 || x5)^(x2 || x6)^(x2 || x7)^(x2 || x8)^(x2 || x9)^(x2 || x10)^(x3 || x4)^(x3 || x5)^(x3 || x6)^(x3 || x7)^(x3 || x8)^(x3 || x9)^(x3 || x10)^(x4 || x5)^(x4 || x6)^(x4 || x7)^(x4 || x8)^(x4 || x9)^(x4 || x10)^(x5 || x6)^(x5 || x7)^(x5 || x8)^(x5 || x9)^(x5 || x10)^(x6 || x7)^(x6 || x8)^(x6 || x9)^(x6 || x10)^(x7 || x8)^(x7 || x9)^(x7 || x10)^(x8 || x9) ^ (x8 || x10) ^ (x9 || x10) ^ (x1 || x2 || x3) ^ (x1 || x2 || x4) ^ (x1 || x2 || x5) ^ (x1 || x2 || x6) ^(x1 || x2 || x7) ^ (x1 || x2 || x8) ^ (x1 || x2 || x9) ^ (x1 || x2 || x10) ^ (x1 || x3 || x4) ^ (x1 || x3 || x5) ^(x1 || x3 || x6) ^ (x1 || x3 || x7) ^ (x1 || x3 || x8) ^ (x1 || x3 || x9) ^ (x1 || x3 || x10) ^ (x1 || x4 || x5) ^(x1 || x4 || x6) ^ (x1 || x4 || x7) ^ (x1 || x4 || x8) ^ (x1 || x4 || x9) ^ (x1 || x4 || x10) ^ (x1 || x5 || x6) ^(x1 || x5 || x7) ^ (x1 || x5 || x8) ^ (x1 || x5 || x9) ^ (x1 || x5 || x10) ^ (x1 || x6 || x7) ^ (x1 || x6 || x8) ^(x1 || x6 || x9) ^ (x1 || x6 || x10) ^ (x1 || x7 || x8) ^ (x1 || x7 || x9) ^ (x1 || x7 || x10) ^ (x1 || x8 || x9) ^(x1 || x8 || x10) ^ (x1 || x9 || x10) ^ (x2 || x3 || x4) ^ (x2 || x3 || x5) ^ (x2 || x3 || x6) ^ (x2 || x3 || x7) ^(x2 || x3 || x8) ^ (x2 || x3 || x9) ^ (x2 || x3 || x10) ^ (x2 || x4 || x5) ^ (x2 || x4 || x6) ^ (x2 || x4 || x7) ^(x2 || x4 || x8) ^ (x2 || x4 || x9) ^ (x2 || x4 || x10) ^ (x2 || x5 || x6) ^ (x2 || x5 || x7) ^ (x2 || x5 || x8) ^(x2 || x5 || x9) ^ (x2 || x5 || x10) ^ (x2 || x6 || x7) ^ (x2 || x6 || x8) ^ (x2 || x6 || x9) ^ (x2 || x6 || x10) ^(x2 || x7 || x8) ^ (x2 || x7 || x9) ^ (x2 || x7 || x10) ^ (x2 || x8 || x9) ^ (x2 || x8 || x10) ^ (x2 || x9 || x10) ^(x3 || x4 || x5) ^ (x3 || x4 || x6) ^ (x3 || x4 || x7) ^ (x3 || x4 || x8) ^ (x3 || x4 || x9) ^ (x3 || x4 || x10) ^(x3 || x5 || x6) ^ (x3 || x5 || x7) ^ (x3 || x5 || x8) ^ (x3 || x5 || x9) ^ (x3 || x5 || x10) ^ (x3 || x6 || x7) ^(x3 || x6 || x8) ^ (x3 || x6 || x9) ^ (x3 || x6 || x10) ^ (x3 || x7 || x8) ^ (x3 || x7 || x9) ^ (x3 || x7 || x10) ^(x3 || x8 || x9) ^ (x3 || x8 || x10) ^ (x3 || x9 || x10) ^ (x4 || x5 || x6) ^ (x4 || x5 || x7) ^ (x4 || x5 || x8) ^(x4 || x5 || x9) ^ (x4 || x5 || x10) ^ (x4 || x6 || x7) ^ (x4 || x6 || x8) ^ (x4 || x6 || x9) ^ (x4 || x6 || x10) ^(x4 || x7 || x8) ^ (x4 || x7 || x9) ^ (x4 || x7 || x10) ^ (x4 || x8 || x9) ^ (x4 || x8 || x10) ^ (x4 || x9 || x10) ^(x5 || x6 || x7) ^ (x5 || x6 || x8) ^ (x5 || x6 || x9) ^ (x5 || x6 || x10) ^ (x5 || x7 || x8) ^ (x5 || x7 || x9) ^(x5 || x7 || x10) ^ (x5 || x8 || x9) ^ (x5 || x8 || x10) ^ (x5 || x9 || x10) ^ (x6 || x7 || x8) ^ (x6 || x7 || x9) ^(x6 || x7 || x10) ^ (x6 || x8 || x9) ^ (x6 || x8 || x10) ^ (x6 || x9 || x10) ^ (x7 || x8 || x9) ^ (x7 || x8 || x10) ^(x7 || x9 || x10) ^ (x8 || x9 || x10);
}

int EXFOR() {
	int n;
	cin >> n;
	while(n--) {
		int x1, x2, x3, x4, x5, x6, x7, x8, x9, x10;
		cin >>x1>>x2>>x3>>x4>>x5>>x6>>x7>>x8>>x9>>x10;
		cout << doEXFOR(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) << endl;
	}
	return 0;
}

int doANARC05B(const vector<int>& v1,const vector<int>& v2) {
	int i1,i2;
	int p1,p2;
	int n1,n2;
	n1= v1.size();
	n2= v2.size();
	p1=p2=0;
	int s=0;
	i1=i2=0;
	while(1) {
		if (i1==n1||i2==n2) {
			while(i1<n1) {
				p1+=v1[i1];
				i1++;
			}
			while(i2<n2) {
				p2+=v2[i2];
				i2++;
			}
			s+=max(p1,p2);
			break;
		}
		if( v1[i1]==v2[i2]) {
			s +=max(p1,p2) + v1[i1];
			p1=p2=0;
			i1++;
			i2++;
		} else if (v1[i1]<v2[i2]){
			p1+=v1[i1];
			i1++;
		} else {
			p2+=v2[i2];
			i2++;
		}
	}
	return s;
}

int ANARC05B() {
	int n;
	while(1) {
		cin >> n;
		if(!n) break;
		vector<int> v1,v2;
		int k;
		while(n--) {
			cin >> k;
			v1.push_back(k);
		}
		cin >> n;
		while(n--) {
			cin >> k;
			v2.push_back(k);
		}
		cout << doANARC05B(v1,v2) << endl;
	}
	return 0;
}

int EGYPIZZA() {
	int n;
	cin >> n;
	string ln;
	int oq,ha,tq;
	oq=ha=tq=0;
	while(n--) {
		do {
			getline(cin,ln);
		}while (ln.find_first_not_of(" \t\r")==string::npos);
//		cout << ln << endl;
		if (ln=="1/4") oq ++;
		if (ln=="1/2") ha ++;
		if (ln=="3/4") tq ++;
	}
//	cout << oq << " " << ha << " " << tq << endl;
	n=1;
	n+=ha/2;
	ha=ha%2;
	int k = min(tq,oq);
	n += k;
	tq -=k;
	oq -=k;
	n+=tq;
	if (ha) {
		if (oq >=2) {
			n+=1;
			oq -=2;
			n+=oq/4;
			oq = oq%4;
			if(oq)
				n+=1;
		} else {
			n+=1;
		}
	} else {
		n+=oq/4;
		oq = oq%4;
		if(oq)
			n+=1;
	}
	cout << n << endl;
	return 0;
}

void doMRECAMAN(int n,vector<int>& a) {
	set<int> s;
	a[0]=0;
	s.insert(0);
	for(int i=1;i<n;i++) {
		int k = a[i-1]-i;
		if(k>0 && s.find(k)==s.end())
			a[i] = k;
		else a[i] = a[i-1]+i;
		s.insert(a[i]);
	}
}

int MRECAMAN() {
	int n=500001;
	vector<int> v(n);
	doMRECAMAN(n,v);
	while(1) {
		int n;
		cin >> n;
		if(n==-1)break;
		cout << v[n] << endl;
	}
	return 0;
}

int main() {
//	findPath(maze,N);
//
//	char s[] = "4+2*3-5/5";
//	cout << s << " = " << eval(s,strlen(s)) << endl;
//
//	int a[] = {1,3,2};
//	vector<int> v(a,a+3);
//	sort(v.begin(),v.end(),myCmp);
//	for(vector<int>::iterator it=v.begin();it!=v.end();++it)
//		cout << *it << " ";
//	cout << endl;
//
//	string str("abc");
//	string str2("abc");
//	cout << str[0] << endl;
//	cout << (str==str2) << endl;
//	cout << str2.append("def") << endl;
//	cout << str2 << endl;
//
//	testCalcProb();
//
//	SellingProducts sp;
//	sp.test();
//
//	JumpingBoard jb;
//	jb.test();
//
////	static map<int,int> m;
////	cout << (m.find(2)==m.end()) << endl;
////	m[2] =3;
////	cout << (m.find(2)==m.end()) << endl;
//
//	RabbitNumbering RN;
//	{
//		int a[] = {25, 489, 76, 98, 704, 98, 768, 39, 697, 8, 56, 74, 36, 95, 87, 2, 968, 4, 920, 54, 873, 90};
////		cout << "len = " << sizeof(a)/sizeof(int) << endl;
//		vector<int> v(a,a+sizeof(a)/sizeof(int));
//		cout << RN.theCount(v) << endl;
//	}
//
//	Nisoku NS;
//	{
//		double a[] = {1.5, 1.7, 1.6, 1.5};
//		vector<double> v(a,a+sizeof(a)/sizeof(double));
//		cout << NS.theMax(v) << endl;
//	}

//	TheBoredomDivOne TBDO;
//	cout << TBDO.find(1,1) << endl;
//	cout << TBDO.find(2,1) << endl;
//	cout << TBDO.find(1,2) << endl;
//	cout << TBDO.find(4,7) << endl;

//	cout << c(1,1) << endl;
//	cout << c(1,2) << endl;
//	cout << c(1,3) << endl;
//	cout << c(1,8) << endl;
//	cout << c(1,100) << endl;
//
//	cout << cs(100) << endl;
//	cout << cs(20000) << endl;

//	p329(500,15,"PPPPNNPPPNPPNPN");
//	p329(500,8,"PPPPNNPP");

//	cout << coutSemiPrime(30) << endl;
//	cout << coutSemiPrime(100000000) << endl;
//	cout << p187(30) << endl;
//	cout << p187(100000000) << endl;

//	cout << "p51() " << p51() << endl;

//	cout << p187(100000000,ps) << endl;

//	cout << a130(7) << endl;
//	cout << a130(41) << endl;
//	cout << a130(27) << endl;
//	cout << a130(91) << endl;
//	cout << a130(259) << endl;
//	cout << "p130() " << p130() << endl;

//	cout << d183(11) << endl;
//	cout << d183(8) << endl;
//	cout << p183(100) << endl;
//	cout << p183(10000) << endl;

//	cout << p100(10) << endl;
//	cout << p100(100) << endl;
//	cout << p100(1000000000000LL) << endl;

//	cout << "TCONNUM" << endl;
//	TCONNUM();

//	cout << "MB1" << endl;
//	MB1();

//	cout << "TDKPRIME" << endl;
//	TDKPRIME();

//	POLEVAL();
//	TOANDFRO();
//	NSTEPS();
//	CANTON();
//	SAMER08F();
//	GNY07A();
//	CANDY();
//	PERMUT2();
//	HANGOVER();
//	COINS();
//	cout << doCOINS(1000000000) << endl;
//	LASTDIG();
//	CANDY3();
//	ACODE();
//	STPAR();
//	ABSYS();
//	AE00();
//	doAE00(6 );
//	doAE00(5000 );
//	doAE00(10000 );
//	vector<int> v;
//	exhaustiveBISHOP(3,0,v);
//	GNY07B();
//	ARMY();
//	AE1B();
//	PIGBANK();
//	NEG2(-13);
//	for(int i=0;i<10;i++)
//		NEG2(i);
//	for(int i=0;i>=-10;i--)
//		NEG2(i);
//	STAMPS();
//	EASYPROB();
//	HELLOKIT();
//	OFFSIDE();
//	FENCE1();
//	CPRMT();
//	FAMILYP();
//	NSYSTEM();
//	QUALITY();
//	ARITH2();
//	BYTESM2();
//	MISERMAN();
//	SUMITR();
//	cout << p78 (5) << endl ;
//	for(int i=1;i<100;i++)
//		cout << p78 (i) << endl ;
//	p78_();
//	cout << g_p86(99) << endl;
//	cout << g_p86(100) << endl;
//	QUADAREA();
//	WORDCNT();
//	ALCHE();
//	MRECAMAN();
//	GIRLSNBS();
//	TRGRID();
//	EXFOR();
//	EGYPIZZA();
	ANARC05B();
	return 0;
}


