//============================================================================
// Name        : tcpp.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <stdio.h>
#include <malloc.h>

int stack[1000];
int top = 0;

int* gale_shapley (int n, int *men_candidates, int *women_rank) {
	int i;
	int w,m,oldm;
	int *men_women_tried, *marriage;
	men_women_tried = (int*) calloc(n,sizeof(int));
	marriage = (int*)malloc(n*sizeof(int));
	for(i=0;i<n;i++) marriage[i]=-1;
	for(i=0;i<n;i++) stack[top++] = i;
	while (1) {
		if (top==0) break;
		m = stack[--top];
		w = men_candidates[m*n+men_women_tried[m]];
		//printf ("m,w = %d,%d\n",m,w);
		oldm = marriage[w];
		if (oldm==-1) {
			marriage[w] = m;
		} else if (women_rank[w*n+m] < women_rank[w*n+oldm]) {
			marriage[w] = m;
			stack[top++] = oldm;
		}
		men_women_tried[m] ++;
	}
	free(men_women_tried);
	return marriage;
}

#define N 3
int main() {
	// A: YXZ  B: ZYX  C: XZY  X: BAC  Y: CBA  Z: ACB
	int men_candidates[] = {1,0,2,2,1,0,0,2,1};
	int women_rank[] = {1,0,2,2,1,0,0,2,1};
	int *marriage;
	int i;

	marriage = gale_shapley (N, men_candidates, women_rank);
	for(i=0;i<N;i++)
		printf ("woman %d => man %d\n",i,marriage[i]);
	free(marriage);
	return 0;
}
