#include<iostream>

using namespace std;

int pre[20000], rank[20000];

void makeset(int x) {
	pre[x] = -1;
	rank[x] = 0;
}

int find(int x) {
	int r = x;
	while (pre[r] != -1)
		r = pre[r];
	while (x != r) {
		int q = pre[x];
		pre[x] = r;
		x = q;
	}
	return r;
}

void unionone(int a, int b) {
	int t1 = find(a);
	int t2 = find(b);
	if (rank[t1] > rank[t2])
		pre[t2] = t1;
	else
		pre[t1] = t2;

	if (rank[t1] == rank[t2])
		rank[t2]++;
}

int main() {
	int N,M;
	int i, a, b, c, d;
	if (cin >> N >> M)
	{
		if (M != N - 1) {
			cout << "NO" << endl;
			return 0;
		}

		for (i = 1; i <= N; i++)
			makeset(i);

		for (i = 1; i <= M; i++) {
			cin >> a >> b;
			if (find(a) != find(b))
				unionone(a, b);
		}

		int i1 = find(1);
		for (i = 1; i <= N; i++) {
			if (find(i) != i1) {
				cout << "NO" << endl;
				return 0;
			}
		}
		cout << "YES" << endl;
	}
	return 0;
}
