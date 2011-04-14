//============================================================================
// Name        : tcpp.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
#include <vector>
#include <list>
#include <map>
using namespace std;

void gale_shapley_aux (
		map<int,int>& marriage,
		list<int>& men,
		list<int>& newmen,
		map<int,list<int> >& men_table,
		map<int,map<int,int> >women_ratings) {
	list<int>::iterator man;
	if (men.empty()) return;
	for(man=men.begin();man!=men.end();++man) {
		list<int>& women = men_table.at(*man);
		int woman = women.front();
		women.pop_front();
		int oldman = marriage.at(woman);
		if (oldman<0) { // If woman not engaged
			marriage.at(woman) = *man;
		} else if (women_ratings.at(woman).at(*man) < women_ratings.at(woman).at(oldman)) {
			marriage.at(woman) = *man;
			newmen.push_front(oldman);
		} else
			newmen.push_front(*man);
	}
}

void gale_shapley (map <int,list<int> > & men_pref,
		map <int,list<int> > & women_pref) {
	map <int,list<int> >::iterator i;
	map<int,int> marriage;
	for(i=women_pref.begin();i!=women_pref.end();++i)
		marriage[(*i).first] = -1;

	list<int> men;
	for(i=men_pref.begin();i!=men_pref.end();++i)
		men.push_front((*i).first);

	map<int,map<int,int> > women_ratings;
	for(i=women_pref.begin();i!=women_pref.end();++i) {
		int woman = (*i).first;
		list<int>& men = (*i).second;
		unsigned int i;
		list<int>::iterator man;
		for (i=0,man=men.begin();i<men.size();i++,++man)
			women_ratings[woman][*man] = i;
	}

	map<int,list<int> > men_table;
	for(i=men_pref.begin();i!=men_pref.end();++i)
		men_table[(*i).first] = (*i).second;

	list<int> newmen;
	gale_shapley_aux(marriage, men, newmen, men_table, women_ratings);

	map<int,int>::iterator p;
	for (p=marriage.begin();p!=marriage.end();++p)
		cout << (*p).first << "->" << (*p).second <<endl;
	return;
}

int main() {
	map <int,list<int> > men_pref;
	map <int,list<int> > women_pref;
	list <int> l;
	l.push_front(3);
	l.push_front(2);
	men_pref[0] = l;

	l.clear();
	l.push_front(2);
	l.push_front(3);
	men_pref[1] = l;

	l.clear();
	l.push_front(1);
	l.push_front(0);
	women_pref[2] = l;

	l.clear();
	l.push_front(0);
	l.push_front(1);
	women_pref[3] = l;

	gale_shapley (men_pref, women_pref);
	return 0;
}
